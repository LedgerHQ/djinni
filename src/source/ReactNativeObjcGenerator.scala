/**
  * Copyright 2014 Dropbox, Inc.
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *    http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */

package djinni

import djinni.ast.Record.DerivingType
import djinni.ast._
import djinni.generatorTools._
import djinni.meta._
import djinni.syntax.Error
import djinni.writer.IndentWriter

import scala.collection.mutable
import scala.collection.parallel.immutable

class ReactNativeObjcGenerator(spec: Spec) extends ObjcGenerator(spec) {

  class ReactNativeRefs() {
    var body = mutable.TreeSet[String]()
    var header = mutable.TreeSet[String]()

    def find(ty: TypeRef, importRCT: Boolean = false) { find(ty.resolved, importRCT) }
    def find(tm: MExpr, importRCT: Boolean) {
      tm.args.foreach(t => find(t, importRCT))
      find(tm.base, importRCT)
    }
    def find(m: Meta, importRCT: Boolean) = for(r <- marshal.reactReferences(m)) r match {
      case ImportRef(arg) => {
        header.add("#import " + arg)
        if (importRCT && !isEnum(MExpr(m, List())) && arg.indexOf("\"") == 0) {
          val rctHeader = s""""${spec.reactNativeTypePrefix}${arg.substring(1)}"""
          header.add("#import " + rctHeader)
        }
      }
      case DeclRef(decl, _) => header.add(decl)
    }
  }

  override def generateEnum(origin: String, ident: Ident, doc: Doc, e: Enum) {

  }

  /**
    * Generate Interface
    */
  override def generateInterface(origin: String, ident: Ident, doc: Doc, typeParams: Seq[TypeParam], i: Interface) {
    val refs = new ReactNativeRefs()
    i.methods.map(m => {
      val addRCTHeader = true
      m.params.map(p => refs.find(p.ty, addRCTHeader))
      m.ret.foreach(r => refs.find(r, addRCTHeader))
    })
    i.consts.map(c => {
      refs.find(c.ty)
    })

    val objcInterface = if(i.ext.objc) marshal.typename(ident, i) + spec.reactNativeObjcImplSuffix else marshal.typename(ident, i)
    val self = spec.reactNativeTypePrefix + marshal.typename(ident, i)
    refs.header.add("#import <Foundation/Foundation.h>")
    refs.header.add("#import <React/RCTBridgeModule.h>")
    refs.header.add("#import <React/RCTBridge.h>")
    //Include
    val pathToObjcImpl = s""""${objcInterface}.h""""
    refs.header.add(s"#import $pathToObjcImpl")

    def writeObjcFuncDecl(method: Interface.Method, w: IndentWriter) {
      val ret = marshal.returnType(method.ret)
      val hasOnlyCallback = method.params.length == 1 && (marshal.paramType(method.params(0).ty).contains("callback") || marshal.paramType(method.params(0).ty).contains("Callback"))
      val hasNoParams = method.params.length == 0 || hasOnlyCallback
      if(method.params.length == 0 && ret == "void") {
        w.wl(s"RCT_EXPORT_METHOD(${idObjc.method(method.ident)}")
      } else {
        val declEnd = s"""${if (!method.static) ":(NSDictionary *)currentInstance " else ""}""" + s"${if (hasNoParams) "" else "withParams"}"
        val decl = s"RCT_REMAP_METHOD(${idObjc.method(method.ident)},${idObjc.method(method.ident)}${declEnd}"
        writeAlignedReactNativeCall(w, decl, method.params, "", p => {
          //No callbacks
          if (!marshal.paramType(p.ty).contains("callback") && !marshal.paramType(p.ty).contains("Callback")) {
            //TODO: case of containers with 'Interface' type
            if (isInterface(p.ty.resolved) ) {
              Some(idObjc.field(p.ident), s"(NSDictionary *)${idObjc.local(p.ident)}")
            } else {
              Some(idObjc.field(p.ident), s"(${marshal.paramType(p.ty)})${idObjc.local(p.ident)}")
            }
          } else {
            None
          }
        })
        //TODO: set hasCallback
        var hasCallback = false
        method.params.foreach(p => hasCallback = hasCallback || marshal.paramType(p.ty).contains("callback") || marshal.paramType(p.ty).contains("Callback"))
        //if(ret != "void" || hasCallback) {
          val begin = if(hasNoParams) "WithResolver" else " withResolver"
          w.w(s"${begin}:(RCTPromiseResolveBlock)resolve rejecter:(RCTPromiseRejectBlock)reject")
        //}
      }
    }

    // Generate the header file for Interface
    val fileName = spec.reactNativeTypePrefix + marshal.headerName(ident)
    writeObjcFile(fileName, origin, refs.header, w => {
      w.wl
      writeDoc(w, doc)
      w.wl(s"@interface $self : NSObject <RCTBridgeModule>")
      w.wl(s"@property (nonatomic, strong) NSMutableDictionary *objcImplementations;")
      w.wl("@end")
    })

    // Generate the implementation file for Interface
    refs.body.add("#import " + q(spec.reactNativeTypePrefix + marshal.headerName(ident)))
    val implfileName = spec.reactNativeTypePrefix + idObjc.ty(ident.name) + ".m"
    writeObjcFile(implfileName, origin, refs.body, w => {
      w.wl
      w.wl(s"@implementation $self")
      w.wl("//Export module")
      w.wl(s"RCT_EXPORT_MODULE($self)")
      w.wl
      w.wl(s"@synthesize bridge = _bridge;")
      w.wl
      w.wl("-(instancetype)init").braced {
        w.wl("self = [super init];")
        w.wl("//Init Objc implementation")
        w.wl("if(self)").braced {
          w.wl(s"self.objcImplementations = [[NSMutableDictionary alloc] init];")
        }
        w.wl("return self;")
      }

      for (m <- i.methods) {
        w.wl
        writeMethodDoc(w, m, idObjc.local)
        writeObjcFuncDecl(m, w)
        w.w(")").braced {
          w.wl

          //Construct call
          val ret = marshal.returnType(m.ret)
          val boxResult = if (m.ret.isDefined) marshal.toBox(m.ret.get.resolved) else false

          if (!m.static) {
            //Get current Instance
            w.wl("""if (!currentInstance[@"uid"] || !currentInstance[@"type"])""").braced {
              w.wl(s"""reject(@"impl_call_error", @"Error while calling $self::${idObjc.method(m.ident)}, first argument should be an instance of ${objcInterface}", nil);""")
            }

            w.wl(s"""${objcInterface} *currentInstanceObj = [self.objcImplementations objectForKey:currentInstance[@"uid"]];""")
            w.wl("if (!currentInstanceObj)").braced {
              w.wl(s"""NSString *error = [NSString stringWithFormat:@"Error while calling ${objcInterface}::${idObjc.method(m.ident)}, instance of uid %@ not found", currentInstance[@"uid"]];""")
              w.wl(s"""reject(@"impl_call_error", error, nil);""")
            }
          }

          //Allows to construct name of RCT classes (cpp or objc implemented interfaces)
          def getRCTName(paramTypeName: String) : String  = {
            if (paramTypeName.indexOf("id<") >= 0 && paramTypeName.indexOf(">") == paramTypeName.length - 1) {
              paramTypeName.slice(paramTypeName.indexOf("<") + 1, paramTypeName.indexOf(">"))
            } else {
              paramTypeName
            }
          }

          //Retrieve from bridge if necessary
          m.params.foreach(p =>{
            if (isInterface(p.ty.resolved)) {
              //TODO: check if parameters are having "type" and "uid" fields
              val index = m.params.indexOf(p)
              //Bridge is shortning prefix if it's starting with RCT
              val prefix = "RCT"
              val paramTypeName = marshal.typename(p.ty)
              val objcParamType = getRCTName(paramTypeName)
              val rctParamType = spec.reactNativeTypePrefix + objcParamType
              val finalObjcParamType = s"$paramTypeName${if (paramTypeName.indexOf("id<") >= 0) "" else " *"}"
              w.wl(s"""$rctParamType *rctParam_${index} = ($rctParamType *)[self.bridge moduleForName:@"${if (rctParamType.indexOf(prefix) == 0) rctParamType.substring(prefix.length) else rctParamType}"];""")
              w.wl(s"""${finalObjcParamType}objcParam_${index} = ($finalObjcParamType)[rctParam_${index}.objcImplementations objectForKey:${idObjc.field(p.ident)}[@"uid"]];""")
              w.wl
            }
          })

          //Start calling Objective-C method
          if (m.static || ret != "void") {
            w.w(s"${marshal.fieldType(m.ret.get)} objcResult = ")
          }
          w.w(s"[${if (m.static) objcInterface else "currentInstanceObj"} ${idObjc.method(m.ident)}")
          //Parameter call
          m.params.foreach(p =>{
            val index = m.params.indexOf(p)
            val start = if (p == m.params(0)) "" else s" ${idObjc.field(p.ident)}"
            val param = if (isInterface(p.ty.resolved))  s"objcParam_${index}" else idObjc.field(p.ident)
            w.w(s"${start}:${param}")
          })

          if(ret != "void") {
            w.wl("];")
            //Add to implementations
            if (m.ret.isDefined && isInterface(m.ret.get.resolved)) {
              w.wl
              w.wl("NSString *uuid = [[NSUUID UUID] UUIDString];")
              if (objcInterface == ret) {
                //Generate a UID
                w.wl("[self.objcImplementations setObject:objcResult forKey:uuid];")
              } else {
                //Check if it's a platform specific implementation (i.e. extCpp = true)
                //This check should rely on a more robust test, go through idls and find corresponding interface and test ?
                val paramTypeName = marshal.typename(m.ret.get)
                val objcParamType = getRCTName(paramTypeName)
                val rctReturn = spec.reactNativeTypePrefix + objcParamType
                //Bridge is shortning prefix if it's starting with RCT
                val prefix = "RCT"
                w.wl(s""" $rctReturn *rctImpl = ($rctReturn *)[self.bridge moduleForName:@"${if (rctReturn.indexOf(prefix) == 0) rctReturn.substring(prefix.length) else rctReturn}"];""")
                w.wl("[rctImpl.objcImplementations setObject:objcResult forKey:uuid];")
              }
              w.wl
              //Construct result
              w.wl(s"""NSDictionary *result = @{@"type" : @"${marshal.typename(m.ret.get)}", @"uid" : uuid };""")
            } else {
              w.w("""NSDictionary *result = @{@"value" : """)
              if (boxResult) {
                w.w("@(")
              }
              w.w("objcResult")
              if (boxResult) {
                w.w(")")
              }
              w.w("};")
            }

            w.wl
            w.wl("if(result)").braced {
              w.wl("resolve(result);")
            }
            w.wl("else").braced {
              //Send a null NSError object for the moment
              w.wl(s"""reject(@"impl_call_error", @"Error while calling ${objcInterface}::${idObjc.method(m.ident)}", nil);""")
            }
          } else w.wl("];")
        }
      }
      w.wl("@end")
    })

  }

  override def generateRecord(origin: String, ident: Ident, doc: Doc, params: Seq[TypeParam], r: Record) {
    val refs = new ReactNativeRefs()
    for (c <- r.consts)
      refs.find(c.ty)
    for (f <- r.fields)
      refs.find(f.ty)

    val objcName = ident.name + (if (r.ext.objc) "_base" else "")
    val noBaseSelf = marshal.typename(ident, r) // Used for constant names
    val self = spec.reactNativeTypePrefix + marshal.typename(objcName, r)
    val fileName = spec.reactNativeTypePrefix + marshal.headerName(ident)

    refs.header.add("#import <Foundation/Foundation.h>")
    refs.header.add("#import <React/RCTBridgeModule.h>")
    refs.header.add("#import <React/RCTBridge.h>")

    val firstInitializerArg = if(r.fields.isEmpty) "" else IdentStyle.camelUpper("with_" + r.fields.head.ident.name)

    // Generate the header file for record
    writeObjcFile(fileName, origin, refs.header, w => {
      writeDoc(w, doc)
      w.wl(s"@interface $self : NSObject <RCTBridgeModule>")
      w.wl(s"@property (nonatomic, strong) NSMutableDictionary *objcImplementations;")
      w.wl
      w.wl("@end")
    })


    // Generate the implementation file for record
    //bodyName(self)
    val implfileName = spec.reactNativeTypePrefix + marshal.typename(ident, r) + ".m"
    writeObjcFile(implfileName, origin, refs.body, w => {
      if (r.consts.nonEmpty) generateObjcConstants(w, r.consts, noBaseSelf, ObjcConstantType.ConstVariable)

      w.wl(s"""#import "$fileName"""")
      w.wl
      w.wl(s"@implementation $self")
      w.wl

      w.wl("//Export module")
      w.wl(s"RCT_EXPORT_MODULE($self)")
      w.wl
      w.wl(s"@synthesize bridge = _bridge;")

      // Constructor from all fields (not copying)
      val init = s"RCT_REMAP_METHOD(init, init$firstInitializerArg"

      writeAlignedObjcCall(w, init, r.fields, "", f => (idObjc.field(f.ident), s"(${marshal.paramType(f.ty)})${idObjc.local(f.ident)}"))
      w.w(")")
      w.wl
      w.wl("@end")
    })
  }

  override def writeObjcFile(fileName: String, origin: String, refs: Iterable[String], f: IndentWriter => Unit) {
    createFile(spec.reactNativeOutFolder.get, fileName, (w: IndentWriter) => {
      w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
      w.wl("// This file generated by Djinni from " + origin)
      w.wl
      if (refs.nonEmpty) {
        // Ignore the ! in front of each line; used to put own headers to the top
        // according to Objective-C style guide
        refs.foreach(s => w.wl(if (s.charAt(0) == '!') s.substring(1) else s))
        w.wl
      }
      f(w)
    })
  }

  def writeAlignedReactNativeCall(w: IndentWriter, call: String, params: Seq[Field], end: String, f: Field => Option[(String, String)]) = {
    w.w(call)
    val skipFirst = new SkipFirst
    params.foreach(p => {
      f(p) match {
        case Some((name, value)) =>
          skipFirst { w.wl; w.w(" " * math.max(0, call.length() - name.length)); w.w(name)  }
          w.w(":" + value)
        case _ =>
      }
    })
    w.w(end)
  }
}
