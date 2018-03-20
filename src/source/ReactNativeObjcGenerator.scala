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

    def find(ty: TypeRef) { find(ty.resolved) }
    def find(tm: MExpr) {
      tm.args.foreach(find)
      find(tm.base)
    }
    def find(m: Meta) = for(r <- marshal.reactReferences(m)) r match {
      case ImportRef(arg) => {
        header.add("#import " + arg)
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
      m.params.map(p => refs.find(p.ty))
      m.ret.foreach(refs.find)
    })
    i.consts.map(c => {
      refs.find(c.ty)
    })

    //if(i.ext.objc) s"${spec.reactNativeTypePrefix}$objcInterface${spec.reactNativeObjcImplSuffix}" else
    val objcInterface = if(i.ext.objc) marshal.typename(ident, i) + spec.reactNativeObjcImplSuffix else marshal.typename(ident, i)
    val self = spec.reactNativeTypePrefix + marshal.typename(ident, i)
    refs.header.add("#import <Foundation/Foundation.h>")
    refs.header.add("#import <React/RCTBridgeModule.h>")

    //Include
    val pathToObjcImpl = if(i.ext.objc) s""""${spec.reactIncludeObjcImpl}/${objcInterface}.h"""" else s""""${spec.reactIncludeObjc}/${objcInterface}.h""""
    refs.header.add(s"#import $pathToObjcImpl")

    def writeObjcFuncDecl(method: Interface.Method, w: IndentWriter) {
      val ret = marshal.returnType(method.ret)
      if(method.params.length == 0 && ret == "void") {
        w.w(s"RCT_EXPORT_METHOD(${idObjc.method(method.ident)}")
      } else {
        val decl = s"RCT_REMAP_METHOD(${idObjc.method(method.ident)},${idObjc.method(method.ident)}"
        writeAlignedObjcCall(w, decl, method.params, "", p => (idObjc.field(p.ident), s"(${marshal.paramType(p.ty)})${idObjc.local(p.ident)}"))
        if(ret != "void") {
          val begin = if(method.params.length == 0) "WithResolver" else " withResolver"
          w.w(s"${begin}:(RCTPromiseResolveBlock)resolve rejecter:(RCTPromiseRejectBlock)reject")
        }
      }
    }

    // Generate the header file for Interface
    val fileName = spec.reactNativeTypePrefix + marshal.headerName(ident)
    writeObjcFile(fileName, origin, refs.header, w => {
      w.wl
      writeDoc(w, doc)
      w.wl(s"@interface $self : NSObject <RCTBridgeModule>")
      w.wl(s"@property (nonatomic, strong) $objcInterface *objcImpl;")
      //w.wl("-(void)initObjcImpl;")
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
      w.wl("-(instancetype)init").braced {
        w.wl("self = [super init];")
        w.wl("//Init Objc implementation")
        w.wl("if(self)").braced {
          w.wl(s"self.objcImpl = [[$objcInterface alloc] init];")
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
          if(ret != "void") {
            w.w("id result = ")
          }
          w.w(s"[${if(m.static) objcInterface else "self.objcImpl"} ${idObjc.method(m.ident)}")

          m.params.foreach(p =>{
            val start = if(p == m.params(0)) "" else s" ${idObjc.field(p.ident)}"
            w.w(s"${start}:${idObjc.field(p.ident)}")
          })
          w.wl("];")

          if(ret != "void") {
            w.wl("if(result)").braced {
              w.wl("resolve(result);")
            }
            w.wl("else").braced {
              //Send a null NSError object for the moment
              w.wl(s"""reject(@"impl_call_error", @"Error while calling ${objcInterface}::${idObjc.method(m.ident)}", nil);""")
            }
          }
        }
      }
      w.wl("@end")
    })

  }

  override def generateRecord(origin: String, ident: Ident, doc: Doc, params: Seq[TypeParam], r: Record) {

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
}
