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

class ReactNativeJavaGenerator(spec: Spec) extends JavaGenerator(spec) {

  class ReactNativeRefs() {

    var java = mutable.TreeSet[String]()

    spec.javaAnnotation.foreach(pkg => java.add(pkg))
    spec.javaNullableAnnotation.foreach(pkg => java.add(pkg))
    spec.javaNonnullAnnotation.foreach(pkg => java.add(pkg))

    def find(ty: TypeRef, importRCT: Boolean = false) { find(ty.resolved, importRCT) }
    def find(tm: MExpr, importRCT: Boolean) {
      tm.args.foreach(t => find(t, importRCT))
      find(tm.base, importRCT)
    }
    def find(m: Meta, importRCT: Boolean) = for(r <- marshal.reactReferences(m)) r match {
      case ImportRef(arg) => {
        java.add(arg)
        if (importRCT && !isEnum(MExpr(m, List())) && arg.indexOf(spec.javaPackage.get) == 0) {
          val rctImport = s"""${spec.reactNativeTypePrefix}${arg.substring(spec.javaPackage.get.length + 1)}"""
          java.add(rctImport)
        }
      }
      case DeclRef(decl, _) => java.add(decl)
    }
  }

  override def generateEnum(origin: String, ident: Ident, doc: Doc, e: Enum) {

  }

  def isExprInterface(tm: MExpr): Boolean = tm.base match {
    case MOptional | MList | MSet | MMap => isExprInterface(tm.args.head)
    case d: MDef =>
      d.defType match {
        case DInterface => true
        case _ => false
      }
    case _ => false
  }

  def isExprRecord(tm: MExpr): Boolean = tm.base match {
    case MOptional | MList | MSet | MMap => isExprRecord(tm.args.head)
    case d: MDef =>
      d.defType match {
        case DRecord => true
        case _ => false
      }
    case _ => false
  }

  def isBinary(tm: MExpr): Boolean = tm.base match {
    case MBinary => true
    case _ => false
  }

  def generateInitMethodForCallback(wr : IndentWriter, callbackType: String, isDeclaration: Boolean = false): Unit = {
    val decl = s"public static $callbackType initWithPromise(Promise promise, (RCTBridge *) bridge)"
    if (isDeclaration) {
      wr.wl(s"$decl;")
    } else {
      wr.wl(decl).braced {
        wr.wl(s"$callbackType callback = new $callbackType();")
        wr.wl("if(callback)").braced {
          wr.wl("callback.promise = promise;")
          wr.wl("callback.bridge = bridge;")
        }
        wr.wl("return callback;")
      }
    }
  }

  /*
  As always, we suppose that callbacks implement only one method: onCallback,
  it has 2 arguments, in order, first one is result and second one error
  */
  def isCallbackInterface(ident: Ident, i: Interface): Boolean = {
    if (marshal.typename(ident, i).contains("Callback")) {
      i.methods.length == 1 && i.methods.filter(m => m.ident.name == "onCallback").length > 0 && i.methods.head.params.length == 2
    } else {
      false
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

  def reactInterfaceType(tm: MExpr) : String = tm.base match {
    case MOptional => s"Option<${reactInterfaceType(tm.args.head)}>"
    case MList => s"ArrayList <${reactInterfaceType(tm.args.head)}> *"
    case MSet => s"HashSet <${reactInterfaceType(tm.args.head)}> *"
    case MMap => s"HashMap <${reactInterfaceType(tm.args(0))},${reactInterfaceType(tm.args(1))}> *"
    case d: MDef =>
      d.defType match {
        case DInterface | DRecord => "HashMap <String, String>"
        case _ => ""
      }
    case _ => ""
  }

  def generateParams(p: Field): Option[(String, String)] = {
    val localIdent = idJava.local(p.ident)
    val identity = idJava.field(p.ident)
    if (isExprInterface(p.ty.resolved) || isExprRecord(p.ty.resolved)) {
      Some(identity, s"${reactInterfaceType(p.ty.resolved)} $localIdent")
    } else {
      Some(identity, s"${marshal.paramType(p.ty)} $localIdent")
    }
  }

  def toReactType(tm: MExpr, converted: String, converting: String, wr: IndentWriter): Unit = tm.base match {
    case MOptional => toReactType(tm.args.head, converted, converting, wr)
    case MList => {
      val paramType = getType(tm.args.head)
      wr.wl(s"ArrayList<$paramType> $converted = new ArrayList<$paramType>();")
      wr.wl(s"for ($paramType ${converting}_elem : $converting)").braced {
        toReactType(tm.args.head, s"${converted}_elem", s"${converting}_elem", wr)
        wr.wl(s"$converted.add(${converted}_elem);")
      }
    }
    case MSet => {
      val paramType = getType(tm.args.head)
      wr.wl(s"Set<$paramType> *$converted = new HashSet<$paramType>();")
      wr.wl(s"for ($paramType ${converting}_elem : arrayFromSet_$converting)").braced {
        toReactType(tm.args.head, s"${converted}_elem", s"${converting}_elem", wr)
        wr.wl(s"$converted.put(${converted}_elem);")
      }
    }
    case MMap => {
      val keyType = getType(tm.args(0))
      val valueType = getType(tm.args(1))
      wr.wl(s"Map<$keyType, $valueType> $converted = HashMap<$keyType, $valueType> ();")
      wr.wl(s"for (Map.Entry<$keyType, $valueType> ${converting}_elem : $converting)").braced {
        wr.wl(s"$keyType ${converted}_elem_key = ${converting}_elem.getKey();")
        wr.wl(s"$valueType ${converted}_elem_value = ${converting}_elem.getValue();")
        toReactType(tm.args.head, s"${converted}_elem_value", s"${converting}_elem_value", wr)
        wr.wl(s"$converted.put(${converted}_elem_key, ${converted}_elem_value);")
      }
    }
    case d: MDef =>
      d.defType match {
        case DInterface | DRecord => {
          wr.wl("String uuid = UUID.randomUUID().toString();")
          val objcParamType = getRCTName(marshal.typename(tm))
          val paramTypeName = spec.reactNativeTypePrefix + objcParamType
          wr.wl(s"""$paramTypeName rctImpl_$converting = ($paramTypeName)self.bridge moduleForName("$paramTypeName");""")
          wr.wl(s"rctImpl_$converting.javaObjects.put(uuid, $converting);")
          wr.wl(s"""Map<String, String> $converted = new HashMap<String, String>();""")
          wr.wl(s"""$converted.put("type","$paramTypeName");""")
          wr.wl(s"""$converted.put("uid",uuid);""")
        }
        case _ =>
      }
    case _ =>
  }

  def getType(tm: MExpr): String = {
    val isItfOrRecord =  isExprInterface(tm) || isExprRecord(tm)
    return s"${if(isItfOrRecord) reactInterfaceType(tm) else marshal.typename(tm)}"
  }

  def fromReactType(tm: MExpr, ident: Ident, converted: String, converting: String, wr: IndentWriter): Unit = tm.base match {
    case MOptional => fromReactType(tm.args.head, ident, converted, converting, wr)
    case MList => {
      //Get types
      val paramType = getType(tm.args.head)
      wr.wl(s"ArrayList<$paramType> $converted = new ArrayList<$paramType>();")
      wr.wl(s"for ($paramType ${converting}_elem : $converting)").braced {
        fromReactType(tm.args(0), ident, s"${converted}_elem", s"${converting}_elem", wr)
        wr.wl(s"$converted.add(${converted}_elem);")
      }
    }
    case MSet => {
      //Get types
      val paramType = getType(tm.args.head)
      wr.wl(s"Set<$paramType> $converted = new HashSet<$paramType>();")
      wr.wl(s"for ($paramType ${converting}_elem : $converting)").braced {
        fromReactType(tm.args(0), ident, s"${converted}_elem", s"${converting}_elem", wr)
        wr.wl(s"$converted.put(${converted}_elem);")
      }
    }
    case MMap => {
      //Get types
      val keyType = getType(tm.args(0))
      val valueType = getType(tm.args(1))
      wr.wl(s"Map<$keyType, $valueType> $converted = new HashMap<$keyType, $valueType>();")
      wr.wl(s"for (Map.Entry<$keyType, $valueType> ${converting}_elem : $converting)").braced {
        wr.wl(s"$keyType ${converted}_elem_key = ${converting}_elem.getKey();")
        wr.wl(s"$valueType ${converted}_elem_value = ${converting}_elem.getValue();")
        fromReactType(tm.args(0), ident, s"${converted}_elem_key", s"${converting}_elem_key", wr)
        fromReactType(tm.args(1), ident, s"${converted}_elem_value", s"${converting}_elem_value", wr)
        wr.wl(s"$converted.put(${converted}_elem_key, ${converted}_elem_value);")
      }
    }
    case d: MDef =>
      d.defType match {
        case DInterface | DRecord => {
          val paramTypeName = marshal.typename(tm)
          val rctParamType = spec.reactNativeTypePrefix + paramTypeName
          //TODO: replace self.bridge by reactContext.getJSModule
          wr.wl(s"""$rctParamType rctParam_${converting} = ($rctParamType)self.bridge.moduleForName("$rctParamType");""")
          wr.wl(s"""${paramTypeName} $converted = ($paramTypeName)rctParam_${converting}.javaObjects.get($converting.get("uid"));""")
        }
        case _ =>
      }
    case _ =>
  }

  override def writeJavaFile(ident: String, origin: String, refs: Iterable[String], f: IndentWriter => Unit) {
    createFile(spec.reactNativeJavaOutFolder.get, spec.reactNativeTypePrefix + idJava.ty(ident) + ".java", (w: IndentWriter) => {
      w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
      w.wl("// This file generated by Djinni from " + origin)
      w.wl
      spec.reactNativeJavaPackage.foreach(s => w.wl(s"package $s;").wl)
      if (refs.nonEmpty) {
        refs.foreach(s => w.wl(s"import $s;"))
        w.wl
      }
      f(w)
    })
  }

  /**
    * Generate Interface
    **/
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

    val callbackInterface = isCallbackInterface(ident, i)
    val javaInterface = if(i.ext.objc) marshal.typename(ident, i) + spec.reactNativeObjcImplSuffix else marshal.typename(ident, i)
    val self = spec.reactNativeTypePrefix + marshal.typename(ident, i)
    refs.java.add("java.util.UUID;")
    refs.java.add("com.facebook.react.bridge.ReactApplicationContext")
    refs.java.add("com.facebook.react.bridge.ReactContextBaseJavaModule")
    refs.java.add(javaInterface)
    writeJavaFile(ident, origin, refs.java, w => {
      writeDoc(w, doc)

      def writeItfMethods() {
        for (m <- i.methods) {
          val hasOnlyCallback = m.params.length == 1 && (marshal.paramType(m.params(0).ty).contains("callback") || marshal.paramType(m.params(0).ty).contains("Callback"))
          val hasNoParams = m.params.length == 0 || hasOnlyCallback
          val firstParam = s"""Map<String, String> currentInstance, """
          writeMethodDoc(w, m, idJava.local)
          val currentMethodName = idJava.method(m.ident)
          if (callbackInterface) {
            val methodIdent = s"public void $currentMethodName("
            writeAlignedReactNativeCall(w, methodIdent, m.params, "", p => {
              val localIdent = idJava.local(p.ident)
              val identity = idJava.field(p.ident)

              Some(identity, s"${marshal.paramType(p.ty)} $localIdent")
            })
            w.w(")")
          } else {
            w.wl("@ReactMethod")

            val methodIdent = s"public void $currentMethodName($firstParam"
            writeAlignedReactNativeCall(w, methodIdent, m.params, "", p => {
              //No callbacks
              if (!marshal.paramType(p.ty).contains("Callback")) {
                generateParams(p)
              } else {
                None
              }
            })
            w.w("Promise promise)")
          }

          val ret = marshal.returnType(m.ret)
          w.w("").braced {
            //Get current Instance
            w.wl("try").braced {

              if (!m.static && !callbackInterface) {
                w.wl("""String sUid = currentInstance.get("uid");""")
                w.wl("""String sType = currentInstance.get("type");""")
                w.wl
                w.wl(s"""$javaInterface currentInstanceObj = self.javaObjects.get("uid");""")
                w.wl("if (!javaObj)").braced {
                  w.wl(s"""throw new Exception("Wrong $self instance passed to $currentMethodName method");""")
                }
                w.wl

                //Retrieve from bridge if necessary
                m.params.foreach(p => {
                  if (isExprInterface(p.ty.resolved) || isExprRecord(p.ty.resolved)) {
                    val index = m.params.indexOf(p)
                    val paramTypeName = marshal.typename(p.ty)
                    val rctParamType = spec.reactNativeTypePrefix + paramTypeName
                    if (paramTypeName.contains("Callback")) {
                      //Construct RCT callback from resolver and rejecter
                      w.wl(s"$rctParamType javaParam_${index} = rctParamType.initWithPromise(promise, self.bridge);")
                    } else {
                      fromReactType(p.ty.resolved, p.ident, s"javaParam_$index", idJava.field(p.ident), w)
                    }
                  }
                })
              } else if (callbackInterface) {
                //Get returned value by callback
                val errorParam = m.params(1)
                w.wl(s"if (${idJava.field(errorParam.ident)})").braced {
                  w.wl(s"self.promise.reject(ERROR, ${idJava.field(errorParam.ident)}.message);")
                }
                val resultParam = m.params(0)
                val isParamInterface = isExprInterface(resultParam.ty.resolved)
                val isParamRecord = isExprRecord(resultParam.ty.resolved)
                if (isParamInterface || isParamRecord) {
                  toReactType(resultParam.ty.resolved, "converted_result", idJava.field(resultParam.ident), w)
                }
                w.wl
                w.wl(s"self.promise.resolve(${if (isParamInterface || isParamRecord) "converted_result" else idJava.field(resultParam.ident)});")
              }

              if (!callbackInterface) {
                //Start calling Java method
                if (m.static || m.ret.isDefined) {
                  w.w(s"${marshal.fieldType(m.ret.get)} javaResult = ")
                }
                w.w(s"${if (m.static) javaInterface else "currentInstanceObj"}.${idJava.method(m.ident)}(")

                //Parameter call
                m.params.foreach(p =>{
                  val index = m.params.indexOf(p)
                  val param = if (isExprInterface(p.ty.resolved) || isExprRecord(p.ty.resolved))  s"javaParam_${index}" else idJava.field(p.ident)
                  w.w(s"$param${if (index != m.params.length - 1) ", " else ""}")
                })

                w.wl(");")
              }

              if(m.ret.isDefined) {
                val javaReturnType = marshal.fieldType(m.ret.get)
                //Add to implementations
                if (m.ret.isDefined && (isExprInterface(m.ret.get.resolved) || isExprRecord(m.ret.get.resolved))) {
                  //Check if it's a platform specific implementation (i.e. extCpp = true)
                  //This check should rely on a more robust test, go through idls and find corresponding interface and test ?
                  val paramTypeName = marshal.typename(m.ret.get)
                  val objcParamType = getRCTName(paramTypeName)
                  val rctReturn = spec.reactNativeTypePrefix + objcParamType
                  w.wl
                  toReactType(m.ret.get.resolved, "result", "objcResult", w)
                } else {
                  w.wl(s"Map<String, $javaReturnType> result = new HashMap<String, $javaReturnType>();")
                  w.wl("""result.put("value", javaResult);""")
                }

                w.wl
                w.wl("if(result)").braced {
                  w.wl("promise.resolve(result);")
                }
                w.wl("else").braced {
                  w.wl(s"""throw new Exception("$self::$currentMethodName : Failed to return $javaReturnType from $currentMethodName method");""")
                }
              }

            }
            w.wl("catch(Exception e)").braced {
              w.wl(s"${if (callbackInterface) "self." else ""}promise.reject(ERROR, e);")
            }
          }
        }
      }


      if(callbackInterface) {
        w.w(s"public class $self extends ${marshal.typename(ident, i)}").braced {
          w.wl("public Promise promise;")
          w.wl("public Bridge bridge;")
          generateInitMethodForCallback(w, self)
          writeItfMethods()
        }
      } else {
        w.w(s"public class $self extends ReactContextBaseJavaModule").braced {
          w.wl
          //React native
          w.wl("private final ReactApplicationContext reactContext;")
          w.wl(s"private Map<String, $javaInterface> javaObjects;")
          w.wl
          w.wl(s"public $self(ReactApplicationContext reactContext)").braced {
            w.wl("super(reactContext);")
            w.wl("this.reactContext = reactContext;")
            w.wl(s"this.javaObjects = new HashMap<String, $javaInterface>();")
          }
          w.wl
          w.wl("@Override")
          w.wl("public String getName()").braced {
            w.wl(s"""return "$self";""")
          }
          w.wl
          writeItfMethods()

        }
      }
    })

  }

  override def generateRecord(origin: String, ident: Ident, doc: Doc, params: Seq[TypeParam], r: Record) {
    val refs = new ReactNativeRefs()
    val addRCTHeader = true
    for (c <- r.consts)
      refs.find(c.ty)
    for (f <- r.fields)
      refs.find(f.ty, addRCTHeader)

    val javaName = if (r.ext.java) (ident.name + "_base") else ident.name
    val javaFinal = if (!r.ext.java && spec.javaUseFinalForRecord) "final " else ""

    val javaInterface = marshal.typename(javaName, r)
    val self = spec.reactNativeTypePrefix + javaInterface
    val fileName = spec.reactNativeTypePrefix + javaInterface

    refs.java.add("java.util.UUID;")
    refs.java.add("com.facebook.react.bridge.ReactApplicationContext")
    refs.java.add("com.facebook.react.bridge.ReactContextBaseJavaModule")
    refs.java.add(javaInterface)

    writeJavaFile(ident, origin, refs.java, w => {
      writeDoc(w, doc)
      w.w(s"public class $self extends ReactContextBaseJavaModule").braced {
        //React native
        w.wl("private final ReactApplicationContext reactContext;")
        w.wl(s"private Map<String, $javaInterface> javaObjects;")
        w.wl
        w.wl(s"public $self(ReactApplicationContext reactContext)").braced {
          w.wl("super(reactContext);")
          w.wl("this.reactContext = reactContext;")
          w.wl(s"this.javaObjects = new HashMap<String, $javaInterface>();")
        }
        w.wl
        w.wl("@Override")
        w.wl("public String getName()").braced {
          w.wl(s"""return "$self";""")
        }

        val methodIdent = "public static void init("
        writeAlignedReactNativeCall(w, methodIdent, r.fields, "", p => {
          //No callbacks
          if (!marshal.paramType(p.ty).contains("Callback")) {
            generateParams(p)
          } else {
            None
          }
        })
        w.w("Promise promise)").braced {

          //Retrieve from bridge if necessary
          r.fields.foreach(f => {
            if (isExprInterface(f.ty.resolved) || isExprRecord(f.ty.resolved)) {
              val index = r.fields.indexOf(f)
              val paramTypeName = marshal.typename(f.ty)
              val rctParamType = spec.reactNativeTypePrefix + paramTypeName
              if (paramTypeName.contains("Callback")) {
                //Construct RCT callback from resolver and rejecter
                w.wl(s"$rctParamType javaParam_${index} = rctParamType.initWithPromise(promise, self.bridge);")
              } else {
                fromReactType(f.ty.resolved, f.ident, s"javaParam_$index", idJava.field(f.ident), w)
              }
            }
          })

          //Start calling Java method
          w.w(s"$javaInterface javaResult = ")
          w.w(s"${javaInterface}.init(")

          //Parameter call
          r.fields.foreach(f =>{
            val index = r.fields.indexOf(f)
            val param = if (isExprInterface(f.ty.resolved) || isExprRecord(f.ty.resolved))  s"javaParam_${index}" else idJava.field(f.ident)
            w.w(s"$param${if (index != r.fields.length - 1) ", " else ""}")
          })

          w.wl(");")

          val rctReturn = spec.reactNativeTypePrefix + javaInterface
          w.wl

          w.wl("String uuid = UUID.randomUUID().toString();")
          val paramTypeName = spec.reactNativeTypePrefix + javaInterface
          w.wl(s"self.bridge.javaObjects.put(uuid, javaResult);")
          w.wl(s"""Map<String, String> finalResult = new HashMap<String, String>();""")
          w.wl(s"""finalResult.put("type","$paramTypeName");""")
          w.wl("""finalResult.put("uid",uuid);""")
          w.wl("promise.resolve(finalResult);")
        }
      }

    })
  }


  def writeAlignedReactNativeCall(w: IndentWriter, call: String, params: Seq[Field], end: String, f: Field => Option[(String, String)]) = {
    w.w(s"$call")
    params.foreach(p => {
      val isLast = params.tail == p
      f(p) match {
        case Some((name, value)) => {
          w.w(value)
          if (!isLast) {
            w.w(", ")
          }
        }
        case _ =>
      }
    })
    w.w(end)
  }
}
