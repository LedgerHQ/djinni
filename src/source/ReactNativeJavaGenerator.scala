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

class ReactNativeJavaGenerator(spec: Spec, javaInterfaces : Seq[String]) extends JavaGenerator(spec) {

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
        val indexPackageStr = arg.indexOf(spec.javaPackage.get)
        if (indexPackageStr == 0) {
          val interfaceName = arg.substring(spec.javaPackage.get.length + 1)

          //Check if we need to import React Native modules
          if (importRCT && !isEnum(MExpr(m, List()))) {
            val rctImport = s"""${spec.reactNativeTypePrefix}$interfaceName"""
            java.add(rctImport)
          }
        }
      }
      case DeclRef(decl, _) => java.add(decl)
    }
  }

  override def generateEnum(origin: String, ident: Ident, doc: Doc, e: Enum) {

  }

  def isInterfaceOrRecord(tm: MExpr): Boolean = tm.base match {
    case MOptional | MList | MSet | MMap => false
    case d: MDef =>
      d.defType match {
        case DInterface => true
        case DRecord => true
        case _ => false
      }
    case _ => false
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

  def isExprBinary(tm: MExpr): Boolean = tm.base match {
    case MOptional | MList | MSet | MMap => isExprBinary(tm.args.head)
    case MBinary => true
    case _ => false
  }

  def isBinary(tm: MExpr): Boolean = tm.base match {
    case MOptional => isBinary(tm.args.head)
    case MBinary => true
    case _ => false
  }

  def isContainer(tm: MExpr): Boolean = tm.base match {
    case MOptional => isContainer(tm.args.head)
    case MList | MSet | MMap => true
    case _ => false
  }

  def generateInitMethodForCallback(wr : IndentWriter, callbackType: String, isDeclaration: Boolean = false): Unit = {
    val decl = s"public static $callbackType initWithPromise(Promise promise, ReactApplicationContext reactContext)"
    if (isDeclaration) {
      wr.wl(s"$decl;")
    } else {
      wr.wl(decl).braced {
        wr.wl(s"$callbackType callback = new $callbackType();")
        wr.wl("callback.promise = promise;")
        wr.wl("callback.reactContext = reactContext;")
        wr.wl("return callback;")
      }
    }
  }

  def generateReleaseMethod(wr : IndentWriter, javaInterface: String): Unit = {
    wr.wl("@ReactMethod")
    wr.wl("public void release(ReadableMap currentInstance, Promise promise)").braced {
      val rctItf = spec.reactNativeTypePrefix + javaInterface
      wr.wl("""String uid = currentInstance.getString("uid");""")
      wr.wl("""if (uid.length() > 0)""").braced {
        wr.wl("this.javaObjects.remove(uid);")
        wr.wl("promise.resolve(0);")
      }
      wr.wl("else").braced {
        wr.wl(s"""promise.reject("Failed to release instance of $rctItf", "First parameter of $rctItf::release should be an instance of $rctItf");""")
      }
    }
  }

  def generateLogInstancesMethod(wr : IndentWriter, javaItf: String): Unit = {
    wr.wl("@ReactMethod")
    wr.wl("public void log(Promise promise)").braced {
      wr.wl("WritableNativeArray result = new WritableNativeArray();")
      wr.wl(s"for (Map.Entry<String, $javaItf> elem : this.javaObjects.entrySet())").braced {
        wr.wl("""result.pushString(elem.getKey());""")
      }
      wr.wl("promise.resolve(result);")
    }
  }

  def generateFlushInstancesMethod(wr : IndentWriter): Unit = {
    wr.wl("@ReactMethod")
    wr.wl("public void flush(Promise promise)").braced {
      wr.wl("this.javaObjects.clear();")
      wr.wl("promise.resolve(0);")
    }
  }

  def generateHexToDataMethod(wr : IndentWriter): Unit = {
    wr.wl("public static byte[] hexStringToByteArray(String hexString)").braced {
      wr.wl("int hexStringLength = hexString.length();")
      wr.wl("byte[] data = new byte[hexStringLength / 2];")
      wr.wl("for (int i = 0; i < hexStringLength; i += 2)").braced {
        wr.wl("data[i / 2] = (byte) ((Character.digit(hexString.charAt(i), 16) << 4) + Character.digit(hexString.charAt(i+1), 16));")
      }
      wr.wl("return data;")
    }
  }

  def generateDataToHexMethod(wr : IndentWriter): Unit = {
    wr.wl("""static final String HEXES = "0123456789ABCDEF";""")
    wr.wl("public static String byteArrayToHexString( byte [] data)").braced {
      wr.wl("if (data == null)").braced {
        wr.wl("return null;")
      }
      wr.wl("final StringBuilder hexStringBuilder = new StringBuilder( 2 * data.length );")
      wr.wl("for ( final byte b : data )").braced {
        wr.wl("hexStringBuilder.append(HEXES.charAt((b & 0xF0) >> 4)).append(HEXES.charAt((b & 0x0F)));")
      }
      wr.wl("return hexStringBuilder.toString();")
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

  //Allows to construct name of RCT classes (cpp or java implemented interfaces)
  def getRCTName(paramTypeName: String) : String  = {
    if (paramTypeName.indexOf("id<") >= 0 && paramTypeName.indexOf(">") == paramTypeName.length - 1) {
      paramTypeName.slice(paramTypeName.indexOf("<") + 1, paramTypeName.indexOf(">"))
    } else {
      paramTypeName
    }
  }

  def reactInterfaceType(tm: MExpr) : String = tm.base match {
    case MOptional => {
      tm.args.head.base match {
        case MList | MSet | MMap => s"Optional<${reactInterfaceType(tm.args.head)}>"
        case d: MDef =>
          d.defType match {
            case DInterface | DRecord => s"Optional<${reactInterfaceType(tm.args.head)}>"
            case _ => reactInterfaceType(tm.args.head)
          }
        case _ => reactInterfaceType(tm.args.head)
      }

    }
    //case MList => s"ArrayList <${reactInterfaceType(tm.args.head)}>"
    case MList => s"ReadableArray"
    case MSet => s"HashSet <${reactInterfaceType(tm.args.head)}>"
    case MMap => s"HashMap <${reactInterfaceType(tm.args(0))},${reactInterfaceType(tm.args(1))}>"
    case d: MDef =>
      d.defType match {
        case DInterface | DRecord => "ReadableMap"
        case _ => ""
      }
    case _ => marshal.paramType(tm)
  }

  def generateParams(p: Field): Option[(String, String)] = {
    val localIdent = idObjc.local(p.ident)
    val ident = idObjc.field(p.ident)
    def generateParamsLocal(tm: MExpr, identity: String, localIdentity: String, hasParentContainer: Boolean = false): Option[(String, String)] = {
      tm.base match {
        case MMap | MList | MSet => generateParamsLocal(tm.args.head, identity, localIdentity, true)
        case MOptional => generateParamsLocal(tm.args.head, identity, localIdentity)
        case d: MDef =>
          d.defType match {
            case DInterface | DRecord => Some(identity, s"${reactInterfaceType(p.ty.resolved)} $localIdentity")
            case DEnum => Some(identity, s"int $localIdentity")
          }
        case MBinary => {
          hasParentContainer match {
            case true => Some(identity, s"ReadableArray $localIdentity")
            case _ => Some(identity, s"String $localIdentity")
          }
        }
        case _ => hasParentContainer match {
          case true => Some(identity, s"ReadableArray $localIdent")
          case _ => Some(identity, s"${marshal.paramType(p.ty)} $localIdent")
        }
      }
    }
    generateParamsLocal(p.ty.resolved, ident, localIdent)
  }

  def appendMethod(tm : MExpr, prefixMethod: String): String = {
    tm.base match {
      case MList | MSet => s"${prefixMethod}Array"
      case MMap => s"${prefixMethod}Map"
      case d: MDef =>
        d.defType match {
          case DInterface | DRecord => s"${prefixMethod}Map"
          case DEnum => s"${prefixMethod}Int"
        }
      case p: MPrimitive => {
        p.idlName match {
          case "bool" => s"${prefixMethod}Boolean"
          case "i64" | "f32" | "f64" => s"${prefixMethod}Double"
          case "i8" | "i16" | "i32"  => s"${prefixMethod}Int"
        }
      }
      case MString | MDate | MBinary => s"${prefixMethod}String"
      case MOptional => appendMethod(tm.args.head, prefixMethod)
      case _ => s"${prefixMethod}Int"
    }
  }
  def pushMethod(tm: MExpr): String = {
    appendMethod(tm, "push")
  }

  def putMethod(tm: MExpr): String = {
    appendMethod(tm, "put")
  }

  def getMethod(tm: MExpr): String = {
    appendMethod(tm, "get")
  }

  def toReactType(tm: MExpr, converted: String, converting: String, wr: IndentWriter, isJavaImplemented: Boolean = false): Unit = {
    //Get types
    val paramType = if(tm.args.length > 0) marshal.typename(tm.args.head) else ""
    val reactParamType = if(tm.args.length > 0) getType(tm.args.head) else ""
    tm.base match {
      case MOptional => toReactType(tm.args.head, converted, converting, wr, isJavaImplemented)
      case MList => {
        wr.wl(s"WritableNativeArray $converted = new WritableNativeArray();")
        wr.wl(s"for ($paramType ${converting}_elem : $converting)").braced {
          toReactType(tm.args.head, s"${converted}_elem", s"${converting}_elem", wr, isJavaImplemented)
          val element = tm.args.head.base match {
            case MBinary => s"${converted}_elem"
            case d: MDef =>
              d.defType match {
                case DInterface | DRecord => s"${converted}_elem"
                case _ => s"${converting}_elem"
              }
            case _ => s"${converting}_elem"
          }
          wr.wl(s"$converted.${pushMethod(tm.args.head)}($element);")
        }
      }
      case MSet => {
        wr.wl(s"WritableNativeArray $converted = new WritableNativeArray();")
        wr.wl(s"for ($paramType ${converting}_elem : arrayFromSet_$converting)").braced {
          toReactType(tm.args.head, s"${converted}_elem", s"${converting}_elem", wr, isJavaImplemented)
          val element = tm.args.head.base match {
            case MBinary => s"${converted}_elem"
            case d: MDef =>
              d.defType match {
                case DInterface | DRecord => s"${converted}_elem"
                case _ => s"${converting}_elem"
              }
            case _ => s"${converting}_elem"
          }
          wr.wl(s"$converted.${pushMethod(tm.args.head)}($element);")
        }
      }
      case MMap => {
        val keyType = paramType
        val reactKeyType = reactParamType
        val valueType = marshal.typename(tm.args(1))
        val reactValueType = getType(tm.args(1))
        wr.wl(s"WritableNativeMap $converted = WritableNativeMap();")
        wr.wl(s"for (Map.Entry<$keyType, $valueType> ${converting}_elem : $converting)").braced {
          wr.wl(s"$keyType ${converted}_elem_key = ${converting}_elem.getKey();")
          wr.wl(s"$valueType ${converted}_elem_value = ${converting}_elem.getValue();")
          toReactType(tm.args.head, s"${converted}_elem_value", s"${converting}_elem_value", wr, isJavaImplemented)
          val element = tm.args.head.base match {
            case MBinary => s"${converted}_elem"
            case d: MDef =>
              d.defType match {
                case DInterface | DRecord => s"${converted}_elem_value"
                case _ => s"${converting}_elem"
              }
            case _ => s"${converting}_elem_value"
          }
          val putMethodStr = putMethod(tm.args(1))
          wr.wl(s"$converted.${putMethodStr}(${converted}_elem_key, $element);")
        }
      }
      case MBinary => {
        wr.wl(s"String $converted = byteArrayToHexString($converting);")
      }
      case MDate => {
        wr.wl(s"""DateFormat ${converting}DateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");""")
        wr.wl(s"String $converted = ${converting}DateFormat.format($converting);")
      }
      case d: MDef =>
        d.defType match {
          case DInterface | DRecord => {
            wr.wl(s"String ${converting}_uuid = UUID.randomUUID().toString();")
            val javaParamType = getRCTName(marshal.typename(tm))
            val paramTypeName = spec.reactNativeTypePrefix + javaParamType
            wr.wl(s"""$paramTypeName rctImpl_$converting = this.reactContext.getNativeModule($paramTypeName.class);""")
            //If Itf is Java implemented we need a cast here
            val finalConverting = if (isJavaImplemented) s"($javaParamType${spec.reactNativeObjcImplSuffix})$converting" else converting
            wr.wl(s"rctImpl_$converting.getJavaObjects().put(${converting}_uuid, $finalConverting);")
            wr.wl(s"""WritableNativeMap $converted = new WritableNativeMap();""")
            wr.wl(s"""$converted.putString("type","$paramTypeName");""")
            wr.wl(s"""$converted.putString("uid",${converting}_uuid);""")
          }
          case _ =>
        }
      case _ =>
    }
  }

  def getType(tm: MExpr): String = {
    val isItfOrRecord =  isExprInterface(tm) || isExprRecord(tm)
    return s"${if(isItfOrRecord) reactInterfaceType(tm) else marshal.typename(tm)}"
  }

  def fromReactType(tm: MExpr, ident: Ident, converted: String, converting: String, wr: IndentWriter, isfromOptional: Boolean = false, dataContainer: String = "", hasParentContainer: Boolean = false, hasReturnValue: Boolean = true): Unit = {
    //Get types
    val paramType = if(tm.args.length > 0) marshal.typename(tm.args.head) else ""
    val reactParamType = if(tm.args.length > 0) getType(tm.args.head) else ""
    //Case of optional
    val convertingCall = if (isfromOptional) s"$converting.get()" else converting
    tm.base match {
      case MOptional => fromReactType(tm.args.head, ident, converted, converting, wr, true, dataContainer, hasParentContainer)
      case MList => {
        wr.wl(s"ArrayList<$paramType> $converted = new ArrayList<$paramType>();")

        if (dataContainer.length > 0) {
          wr.wl(s"WritableNativeArray ${converted}_data = new WritableNativeArray();")
          wr.wl
        }
        wr.wl(s"for (int i = 0; i <  $convertingCall.size(); i++)").braced {
        wr.wl(s"${if (isBinary(tm.args.head)) "String" else reactParamType} ${converting}_elem = $convertingCall.${getMethod(tm.args(0))}(i);")
        fromReactType(tm.args(0), ident, s"${converted}_elem", s"${converting}_elem", wr, false, s"${converted}_data", true)
          val element = tm.args.head.base match {
            case d: MDef =>
              d.defType match {
                case DInterface | DRecord => s"${converted}_elem"
                case _ => s"${converting}_elem"
              }
            case MBinary => s"${converted}_elem"
            case _ => s"${converting}_elem"
          }

          wr.wl(s"$converted.add($element);")
        }

        if (dataContainer.length > 0) {
          wr.wl(s"""$dataContainer.putArray("${idJava.field(ident)}", ${converted}_data);""")
          wr.wl
        }
      }
      case MSet => {

        if (dataContainer.length > 0) {
          wr.wl(s"WritableNativeArray ${converted}_data = new WritableNativeArray();")
          wr.wl
        }

        wr.wl(s"Set<$paramType> $converted = new HashSet<$paramType>();")
        wr.wl(s"for ($reactParamType ${converting}_elem : $convertingCall)").braced {
          fromReactType(tm.args(0), ident, s"${converted}_elem", s"${converting}_elem", wr, false, s"${converted}_data", true)

          val element = tm.args.head.base match {
            case d: MDef =>
              d.defType match {
                case DInterface | DRecord => s"${converted}_elem"
                case _ => s"${converting}_elem"
              }
            case _ => s"${converting}_elem"
          }

          wr.wl(s"$converted.put($element);")
        }

        if (dataContainer.length > 0) {
          wr.wl(s"""$dataContainer.putArray("${idJava.field(ident)}", ${converted}_data);""")
          wr.wl
        }
      }
      case MMap => {

        if (dataContainer.length > 0) {
          wr.wl(s"WritableNativeArray ${converted}_data = new WritableNativeArray();")
          wr.wl
        }
        //Get types
        val keyType = paramType
        val reactKeyType = reactParamType
        val valueType = marshal.typename(tm.args(1))
        val reactValueType = getType(tm.args(1))
        wr.wl(s"Map<$keyType, $valueType> $converted = new HashMap<$keyType, $valueType>();")
        wr.wl(s"for (Map.Entry<$reactKeyType, $reactValueType> ${converting}_elem : $convertingCall)").braced {
          wr.wl(s"$reactKeyType ${converted}_elem_key = ${converting}_elem.getKey();")
          wr.wl(s"$reactValueType ${converted}_elem_value = ${converting}_elem.getValue();")
          fromReactType(tm.args(0), ident, s"${converted}_elem_key", s"${converting}_elem_key", wr, false, s"${converted}_data", true)
          fromReactType(tm.args(1), ident, s"${converted}_elem_value", s"${converting}_elem_value", wr, false, s"${converted}_data", true)

          val keyElement = tm.args(0).base match {
            case d: MDef =>
              d.defType match {
                case DInterface | DRecord => s"${converted}_elem_key"
                case _ => s"${converting}_elem_key"
              }
            case _ => s"${converting}_elem_key"
          }

          val valueElement = tm.args(1).base match {
            case d: MDef =>
              d.defType match {
                case DInterface | DRecord => s"${converted}_elem_value"
                case _ => s"${converting}_elem_value"
              }
            case _ => s"${converting}_elem_value"
          }

          wr.wl(s"$converted.put($keyElement, $valueElement);")
        }

        if (dataContainer.length > 0) {
          wr.wl(s"""$dataContainer.putArray("${idJava.field(ident)}", ${converted}_data);""")
          wr.wl
        }

      }
      case MBinary => {
        wr.wl(s"byte [] $converted = hexStringToByteArray($converting);")
        wr.wl
      }
      case d: MDef =>
        d.defType match {
          case DInterface | DRecord => {
            val paramTypeName = marshal.typename(tm)
            val rctParamType = spec.reactNativeTypePrefix + paramTypeName
            val isJavaImplemented = javaInterfaces.contains(paramTypeName)

            wr.wl(s"""$rctParamType rctParam_${converting} = this.reactContext.getNativeModule($rctParamType.class);""")
            wr.wl(s"""${paramTypeName} $converted = rctParam_${converting}.getJavaObjects().get($convertingCall.getString("uid"));""")

            if (!hasReturnValue && isJavaImplemented) {
              //Needs conversion to impl type
              wr.wl(s"$paramTypeName${spec.reactNativeObjcImplSuffix} ${converted}_java = ($paramTypeName${spec.reactNativeObjcImplSuffix})$converted;")
              wr.wl(s"${converted}_java.setPromise(promise);")
            }

            if (dataContainer.length > 0 && hasParentContainer) {
              wr.wl(s"""$dataContainer.pushString($convertingCall.getString("uid"));""")
            }
            else if (dataContainer.length > 0) {
              wr.wl(s"""$dataContainer.putString("${idJava.field(ident)}", $convertingCall.getString("uid"));""")
            }

          }
          case DEnum => {
            val paramTypeName = marshal.typename(tm)
            wr.wl(s"if ($converting < 0 || $paramTypeName.values().length <= $converting)").braced {
              wr.wl(s"""promise.reject("Enum error", "Failed to get enum $paramTypeName");""")
              wr.wl("return;")
            }

            wr.wl(s"$paramTypeName $converted = $paramTypeName.values()[$converting];")
          }
          case _ =>
        }
      case _ =>
    }
  }


  def getReturnType(ret: Option[TypeRef]): String = {
    if (ret.isDefined) {
      ret.get.resolved.base match {
        case p: MPrimitive => p.jBoxed
        case _ => marshal.returnType(ret)
      }
    } else {
      "void"
    }
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

  override def generate(idl: Seq[TypeDecl]): Unit = {

    val packageName = spec.reactNativeTypePrefix + "BindingPackage"
    super.generate(idl)
    createFile(spec.reactNativeJavaOutFolder.get, packageName + ".java", (w: IndentWriter) => {
      w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
      w.wl("// This file generated by Djinni")
      w.wl
      w.wl(s"package ${spec.reactNativeJavaPackage.get};")
      w.wl("import java.util.Arrays;")
      w.wl("import java.util.List;")
      w.wl("import java.util.Collections;")
      w.wl("import com.facebook.react.ReactPackage;")
      w.wl("import com.facebook.react.bridge.NativeModule;")
      w.wl("import com.facebook.react.bridge.ReactApplicationContext;")
      w.wl("import com.facebook.react.uimanager.ViewManager;")
      w.wl("import com.facebook.react.bridge.JavaScriptModule;")
      w.wl

      //Collect only itfs and records
      val itfs = idl.collect { case td: InternTypeDecl => td }.collect { td => td.body match {
        case Interface(_,_,_,_) | Record(_,_,_,_) => td
      }}

      w.wl(s"public class $packageName implements ReactPackage").braced {

        w.wl("@Override")
        w.wl("public List<NativeModule> createNativeModules(ReactApplicationContext reactContext)").braced {
          w.w("return Arrays.<NativeModule>asList(")

          itfs.foreach(td => {
            val index = itfs.indexOf(td)
            val itf = spec.reactNativeTypePrefix + marshal.typename(td.ident, td.body)
            if (!itf.contains("Callback")) {
              w.w(s"new $itf(reactContext)")
              if(index != itfs.length - 1) {
                w.w(",")
                w.wl
              }
            }
          })
          w.w(");")
        }
        w.wl
        w.wl("public List<Class<? extends JavaScriptModule>> createJSModules()").braced {
          w.wl("return Collections.emptyList();")
        }
        w.wl
        w.wl("@Override")
        w.wl("public List<ViewManager> createViewManagers(ReactApplicationContext reactContext)").braced {
          w.wl("return Collections.emptyList();")
        }

      }

    });
  }
  
  def addDefaultReferences(references: ReactNativeRefs): Unit = {
    references.java.add("java.util.ArrayList")
    references.java.add("java.util.HashMap")
    references.java.add("java.util.HashSet")
    references.java.add("java.util.Map")
    references.java.add("java.util.Optional")
    references.java.add("java.util.UUID")
    references.java.add("java.text.DateFormat")
    references.java.add("java.text.SimpleDateFormat")
    references.java.add("java.util.Date")
    references.java.add("com.facebook.react.bridge.ReactApplicationContext")
    references.java.add("com.facebook.react.bridge.ReactContextBaseJavaModule")
    references.java.add("com.facebook.react.bridge.ReactContext")
    references.java.add("com.facebook.react.bridge.ReactMethod")
    references.java.add("com.facebook.react.bridge.Promise")
    references.java.add("com.facebook.react.bridge.ReadableMap")
    references.java.add("com.facebook.react.bridge.ReadableArray")
    references.java.add("com.facebook.react.bridge.ReadableNativeMap")
    references.java.add("com.facebook.react.bridge.ReadableNativeArray")
    references.java.add("com.facebook.react.bridge.WritableNativeMap")
    references.java.add("com.facebook.react.bridge.WritableNativeArray")

  }
  /**
    * Generate Interface
    **/
  override def generateInterface(origin: String, ident: Ident, doc: Doc, typeParams: Seq[TypeParam], i: Interface) {
    val refs = new ReactNativeRefs()
    i.methods.map(m => {
      val addRCTHeader = false
      m.params.map(p => refs.find(p.ty, addRCTHeader))
      m.ret.foreach(r => refs.find(r, addRCTHeader))
    })
    i.consts.map(c => {
      refs.find(c.ty)
    })

    //Need for converter from hex string to NSData ?
    val needHexConverter = i.methods.filter(m => {
      m.params.filter(p => {
        p.ty.resolved.base match {
          case MList | MSet | MMap | MOptional => isBinary(p.ty.resolved.args.head)
          case _ => isBinary(p.ty.resolved)
        }
      }).length > 0
    }).length > 0

    val callbackInterface = isCallbackInterface(ident, i)
    val javaInterface = if(i.ext.java) marshal.typename(ident, i) + spec.reactNativeObjcImplSuffix else marshal.typename(ident, i)
    val self = spec.reactNativeTypePrefix + marshal.typename(ident, i)

    //Add default imports (Maps, Arrays, ReactBrigde ...)
    addDefaultReferences(refs)

    if (callbackInterface) {
      refs.java.add(s"${spec.javaPackage.get}.${marshal.typename(ident, i)}")
    } else {
      refs.java.add(s"${spec.javaPackage.get}.${marshal.typename(ident, i)}")
    }

    writeJavaFile(ident, origin, refs.java, w => {
      writeDoc(w, doc)

      def writeItfMethods() {
        for (m <- i.methods) {
          val hasOnlyCallback = m.params.length == 1 && (marshal.paramType(m.params(0).ty).contains("callback") || marshal.paramType(m.params(0).ty).contains("Callback"))
          val hasNoParams = m.params.length == 0 || hasOnlyCallback
          val firstParam = s"""ReadableMap currentInstance${if (hasNoParams) "" else ", "}"""
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

            val methodIdent = if (m.static) s"public void $currentMethodName(" else s"public void $currentMethodName($firstParam"
            writeAlignedReactNativeCall(w, methodIdent, m.params, "", p => {
              //No callbacks
              if (!marshal.paramType(p.ty).contains("Callback")) {
                generateParams(p)
              } else {
                None
              }
            })
            def isParamCallback(typeRef : TypeRef) : Boolean = {
              return marshal.paramType(typeRef).contains("callback") || marshal.paramType(typeRef).contains("Callback")
            }
            val paramIsCallback = m.params.length == 1 && isParamCallback(m.params.head.ty)
            val dropComma = (m.static && (m.params.length == 0 || paramIsCallback)) || (!m.static && m.params.length > 1 && isParamCallback(m.params.reverse.head.ty))
            w.w(s"${if(dropComma) "" else ", "}Promise promise)")
          }

          val ret = marshal.returnType(m.ret)
          w.w("").braced {
            //Get current Instance
            w.wl("try").braced {

              if (!callbackInterface) {
                if(!m.static) {
                  w.wl("""String sUid = currentInstance.getString("uid");""")
                  w.wl
                  w.wl(s"""$javaInterface currentInstanceObj = this.javaObjects.get(sUid);""")
                  w.wl
                }

                def getConverter(param: Field, paramIndex: Int) : Unit = {
                  val paramdIdentity = idJava.field(param.ident)
                  val dataContainer = ""
                  val hasParentContainer = false
                  def getConverterLocal(tm: MExpr, isOptional: Boolean = false) : Unit = tm.base match {
                    case MList | MSet | MMap | MBinary => fromReactType(tm, param.ident, s"javaParam_$paramIndex", idJava.field(param.ident), w, isOptional, dataContainer, hasParentContainer, ret != "void")
                    case MOptional => getConverterLocal(tm.args.head, true)
                    case d: MDef =>
                      d.defType match {
                        case DInterface | DRecord | DEnum => fromReactType(tm, param.ident, s"javaParam_$paramIndex", idJava.field(param.ident), w, isOptional, dataContainer, hasParentContainer, ret != "void")
                        case _ =>
                      }
                    case _ =>
                  }
                  getConverterLocal(param.ty.resolved)
                }

                //Retrieve from bridge if necessary
                m.params.foreach(p => {
                  val index = m.params.indexOf(p)
                  val paramTypeName = marshal.typename(p.ty)
                  val rctParamType = spec.reactNativeTypePrefix + paramTypeName
                  if (paramTypeName.contains("Callback")) {
                    //Construct RCT callback from resolver and rejecter
                    w.wl(s"$rctParamType javaParam_${index} = $rctParamType.initWithPromise(promise, this.reactContext);")
                  } else {
                    val dataContainer = ""
                    val hasParentContainer = false
                    getConverter(p, index)
                  }
                })
              } else {
                //Get returned value by callback
                val errorParam = m.params(1)
                //We suppose that errors are records with message and error fields
                val errorParamField = idJava.field(errorParam.ident)
                w.wl(s"if ($errorParamField != null && $errorParamField.getMessage().length() > 0)").braced {
                  w.wl(s"this.promise.reject(${idJava.field(errorParam.ident)}.toString(), $errorParamField.getMessage());")
                }
                val resultParam = m.params(0)
                val isParamInterface = isExprInterface(resultParam.ty.resolved)
                val isParamRecord = isExprRecord(resultParam.ty.resolved)
                val isParamBinary = isBinary(resultParam.ty.resolved)
                toReactType(resultParam.ty.resolved, "converted_result", idJava.field(resultParam.ident), w)
                w.wl
                w.wl(s"this.promise.resolve(${if (isParamInterface || isParamRecord || isParamBinary) "converted_result" else idJava.field(resultParam.ident)});")
              }

              if (!callbackInterface) {
                //Start calling Java method
                if (m.static || m.ret.isDefined) {
                  w.w(s"${marshal.fieldType(m.ret.get)} javaResult = ")
                }
                w.w(s"${if (m.static) javaInterface else "currentInstanceObj"}.${idJava.method(m.ident)}(")

                //Parameter call
                val hasCallbackParam = if(m.params.length > 0) marshal.paramType(m.params.reverse.head.ty).contains("Callback") else false
                m.params.foreach(p =>{
                  val index = m.params.indexOf(p)
                  def getParamName(tm: MExpr) : String = tm.base match {
                    case MOptional => getParamName(tm.args.head)
                    case MList | MSet | MMap | MBinary => s"javaParam_${index}"
                    case d: MDef =>
                      d.defType match {
                        case DInterface | DRecord | DEnum => s"javaParam_${index}"
                      }
                    case _ => idJava.field(p.ident)
                  }
                  val param = getParamName(p.ty.resolved)
                  w.w(s"$param${if (index != m.params.length - 1) ", " else ""}")
                })

                w.wl(");")
              }

              if(m.ret.isDefined) {
                val javaReturnType = getReturnType(m.ret)
                //Add to implementations
                if (m.ret.isDefined && (isExprInterface(m.ret.get.resolved) || isExprRecord(m.ret.get.resolved))) {
                  //Check if it's a platform specific implementation (i.e. extCpp = true)
                  //This check should rely on a more robust test, go through idls and find corresponding interface and test ?
                  val paramTypeName = marshal.typename(m.ret.get)
                  val javaParamType = getRCTName(paramTypeName)
                  val rctReturn = spec.reactNativeTypePrefix + javaParamType
                  w.wl
                  toReactType(m.ret.get.resolved, "result", "javaResult", w, javaInterfaces.contains(javaReturnType))
                } else {
                  w.wl(s"WritableNativeMap result = new WritableNativeMap();")

                  def getFinalResult(tm: MExpr, converting: String) : String = {
                    val paramType = if(tm.args.length > 0) marshal.typename(tm.args.head) else ""
                    tm.base match {
                      case d: MDef =>
                        d.defType match {
                          case DEnum => {
                            w.wl(s"int finalJavaResult = $converting.ordinal();")
                            "finalJavaResult"
                          }
                          case _ => converting
                        }
                      case MDate => {
                        w.wl("""DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");""")
                        w.wl(s"String finalJavaResult = dateFormat.format($converting);")
                        "finalJavaResult"
                      }
                      case MBinary => {
                        w.wl(s"String finalJavaResult = new String($converting);")
                        "finalJavaResult"
                      }
                      case MOptional => getFinalResult(tm.args.head, converting)
                      case MList | MSet => {
                        val pushMethodStr = pushMethod(tm.args.head)
                        w.wl(s"WritableNativeArray ${converting}_list = new WritableNativeArray();")
                        w.wl(s"for($paramType ${converting}_elem : $converting)").braced {
                          val convertedElem = getFinalResult(tm.args.head, s"${converting}_elem")
                          w.wl(s"${converting}_list.${pushMethodStr}($convertedElem);")
                        }
                        s"${converting}_list"
                      }
                      case MMap => {
                        val keyType = paramType
                        val valueType = marshal.typename(tm.args(1))
                        val putMethodStr = putMethod(tm.args.head)

                        w.wl(s"WritableNativeMap ${converting}_map = new WritableNativeMap();")
                        w.wl(s"for($keyType ${converting}_key : $converting.keySet())").braced {
                          w.wl(s"$valueType ${converting}_elem_value = ${converting}.get(${converting}_key);")
                          val convertedElem = getFinalResult(tm.args.head, s"${converting}_elem_value")
                          w.wl(s"${converting}_map.${putMethodStr}(${converting}_key, $convertedElem);")
                        }
                        s"${converting}_map"
                      }
                      case _ => converting
                    }
                  }
                  val finalJavaResult = getFinalResult(m.ret.get.resolved, "javaResult")
                  w.wl(s"""result.${putMethod(m.ret.get.resolved)}("value", $finalJavaResult);""")
                }

                w.wl
                w.wl("promise.resolve(result);")
              }

            }
            w.wl("catch(Exception e)").braced {
              w.wl(s"${if (callbackInterface) "this." else ""}promise.reject(e.toString(), e.getMessage());")
            }
          }
        }
      }


      if(callbackInterface) {
        w.w(s"public class $self extends ${marshal.typename(ident, i)}").braced {
          w.wl("public Promise promise;")
          w.wl("public ReactApplicationContext reactContext;")
          generateInitMethodForCallback(w, self)
          //Generate hex converter
          if (needHexConverter) {
            generateDataToHexMethod(w)
          }
          writeItfMethods()
        }
      } else {
        w.w(s"public class $self extends ReactContextBaseJavaModule").braced {
          w.wl
          //React native
          w.wl("private final ReactApplicationContext reactContext;")
          w.wl(s"private Map<String, $javaInterface> javaObjects;")
          w.wl(s"public Map<String, $javaInterface> getJavaObjects()").braced {
            w.wl("return javaObjects;")
          }

          w.wl
          w.wl(s"public $self(ReactApplicationContext reactContext)").braced {
            w.wl("super(reactContext);")
            w.wl("this.reactContext = reactContext;")
            w.wl(s"this.javaObjects = new HashMap<String, $javaInterface>();")
            w.wl("WritableNativeMap.setUseNativeAccessor(true);")
          }
          w.wl
          w.wl("@Override")
          w.wl("public String getName()").braced {
            w.wl(s"""return "$self";""")
          }

          val paramTypeName = marshal.typename(ident, i)
          if (javaInterfaces.contains(marshal.typename(ident, i))) {
            //New method
            w.wl(s"@ReactMethod")
            w.wl(s"public void newInstance(Promise promise)").braced {
              w.wl(s"$javaInterface newInstance = new $javaInterface(this.reactContext);")
              w.wl("String uuid = UUID.randomUUID().toString();")
              w.wl(s"this.javaObjects.put(uuid, newInstance);")
              w.wl(s"""WritableNativeMap finalResult = new WritableNativeMap();""")
              w.wl(s"""finalResult.putString("type","$self");""")
              w.wl("""finalResult.putString("uid",uuid);""")
              w.wl("promise.resolve(finalResult);")
            }
          }

          //Release to remove java instance from self.javaOjbects
          generateReleaseMethod(w, marshal.typename(ident, i))
          //Returns uid of all java instances
          generateLogInstancesMethod(w, javaInterface)
          //Flush all java intances from React Native Module's javaObjects attribute
          generateFlushInstancesMethod(w)
          //Generate hex converter
          if (needHexConverter) {
            generateHexToDataMethod(w)
            generateDataToHexMethod(w)
          }
          w.wl
          writeItfMethods()

        }
      }
    })

  }

  override def generateRecord(origin: String, ident: Ident, doc: Doc, params: Seq[TypeParam], r: Record) {
    val refs = new ReactNativeRefs()
    val addRCTHeader = false
    for (c <- r.consts)
      refs.find(c.ty)
    for (f <- r.fields)
      refs.find(f.ty, addRCTHeader)

    //Need for converter from hex string to NSData ?
    val needHexConverter = r.fields.filter(f => f.ty.resolved.base match {
      case MList | MSet | MMap | MOptional => isBinary(f.ty.resolved.args.head)
      case _ => isBinary(f.ty.resolved)
    }).length > 0

    val javaName = if (r.ext.java) (ident.name + "_base") else ident.name
    val javaFinal = if (!r.ext.java && spec.javaUseFinalForRecord) "final " else ""

    val javaInterface = marshal.typename(javaName, r)
    val self = spec.reactNativeTypePrefix + javaInterface
    val fileName = spec.reactNativeTypePrefix + javaInterface

    val hasOneFieldAsInterface = r.fields.filter(f => isExprInterface(f.ty.resolved) || isExprRecord(f.ty.resolved)).length > 0

    addDefaultReferences(refs)

    refs.java.add(s"${spec.javaPackage.get}.${javaInterface}")

    writeJavaFile(ident, origin, refs.java, w => {
      writeDoc(w, doc)
      w.w(s"public class $self extends ReactContextBaseJavaModule").braced {
        //React native
        w.wl("private final ReactApplicationContext reactContext;")
        w.wl(s"private Map<String, $javaInterface> javaObjects;")
        if (hasOneFieldAsInterface) {
          w.wl(s"private WritableNativeMap implementationsData;")
        }
        w.wl(s"public Map<String, $javaInterface> getJavaObjects()").braced {
          w.wl("return javaObjects;")
        }
        w.wl
        w.wl(s"public $self(ReactApplicationContext reactContext)").braced {
          w.wl("super(reactContext);")
          w.wl("this.reactContext = reactContext;")
          w.wl(s"this.javaObjects = new HashMap<String, $javaInterface>();")
          w.wl("WritableNativeMap.setUseNativeAccessor(true);")
          if (hasOneFieldAsInterface) {
            w.wl(s"this.implementationsData = new WritableNativeMap();")
          }
        }
        w.wl
        w.wl("@Override")
        w.wl("public String getName()").braced {
          w.wl(s"""return "$self";""")
        }

        //Release to remove java instance from self.javaOjbects
        generateReleaseMethod(w, marshal.typename(ident, r))
        //Returns uid of all java instances
        generateLogInstancesMethod(w, javaInterface)
        //Flush all java intances from React Native Module's javaObjects attribute
        generateFlushInstancesMethod(w)
        //Generate hex converter
        if (needHexConverter) {
          generateHexToDataMethod(w)
          generateDataToHexMethod(w)
        }
        w.wl
        w.wl("@ReactMethod")
        val methodIdent = "public void init("
        writeAlignedReactNativeCall(w, methodIdent, r.fields, "", p => {
          //No callbacks
          if (!marshal.paramType(p.ty).contains("Callback")) {
            generateParams(p)
          } else {
            None
          }
        })
        w.w(", Promise promise)").braced {

          if (hasOneFieldAsInterface) {
            w.wl("WritableNativeMap implementationsData = new WritableNativeMap();")
          }

          //Retrieve from bridge if necessary
          r.fields.foreach(f => {
            val index = r.fields.indexOf(f)
            val paramTypeName = marshal.typename(f.ty)
            val rctParamType = spec.reactNativeTypePrefix + paramTypeName
            if (paramTypeName.contains("Callback")) {
              //Construct RCT callback from resolver and rejecter
              w.wl(s"$rctParamType javaParam_${index} = rctParamType.initWithPromise(promise, this.reactContext);")
            } else {
              val dataContainer = if (isExprInterface(f.ty.resolved) || isExprRecord(f.ty.resolved)) "implementationsData" else ""
              fromReactType(f.ty.resolved, f.ident, s"javaParam_$index", idJava.field(f.ident), w, false, dataContainer)
            }
          })

          //Start calling Java method
          w.w(s"$javaInterface javaResult = ")
          w.w(s"new $javaInterface(")

          //Parameter call
          r.fields.foreach(f =>{
            val index = r.fields.indexOf(f)

            def getParamName(field: Field, fieldIndex: Int) : String = {
              val fieldIdentity = idJava.field(field.ident)
              def getParamNameLocal(tm: MExpr) : String = tm.base match {
                case MOptional => getParamNameLocal(tm.args.head)
                case MList | MSet | MMap | MBinary => s"javaParam_$fieldIndex"
                case d: MDef =>
                  d.defType match {
                    case DInterface | DRecord | DEnum => s"javaParam_$fieldIndex"
                  }
                case _ => fieldIdentity
              }
              getParamNameLocal(field.ty.resolved)
            }
            val param = getParamName(f, index)
            w.w(s"$param${if (index != r.fields.length - 1) ", " else ""}")
          })

          w.wl(");")

          val rctReturn = spec.reactNativeTypePrefix + javaInterface
          w.wl

          w.wl("String uuid = UUID.randomUUID().toString();")
          val paramTypeName = spec.reactNativeTypePrefix + javaInterface
          w.wl(s"this.javaObjects.put(uuid, javaResult);")
          w.wl(s"""WritableNativeMap finalResult = new WritableNativeMap();""")
          w.wl(s"""finalResult.putString("type","$paramTypeName");""")
          w.wl("""finalResult.putString("uid",uuid);""")
          if (hasOneFieldAsInterface) {
            w.wl(s"this.implementationsData.putMap(uuid, implementationsData);")
          }
          w.wl("promise.resolve(finalResult);")
        }

      //Mapping method
      if (hasOneFieldAsInterface) {
        w.wl(s"public void mapImplementationsData(ReadableMap currentInstance)").braced {
          w.wl("""String currentInstanceUid = currentInstance.getString("uid");""")
          w.wl(s"""$javaInterface javaImpl = this.javaObjects.get(currentInstanceUid);""")
          w.wl("WritableNativeMap implementationsData = new WritableNativeMap();")
          r.fields.map(f => {
            val id = r.fields.indexOf(f)
            val isFieldInterfaceOrRecord = isExprInterface(f.ty.resolved) || isExprRecord(f.ty.resolved)
            val fieldIdent = idJava.field(f.ident)
            val suffix = fieldIdent.substring(0, 1).toUpperCase() + fieldIdent.substring(1)
            val getterName = s"get$suffix"
            if (isFieldInterfaceOrRecord) {
              w.wl(s"${marshal.typename(f.ty.resolved)} field_$id = javaImpl.${getterName}();")
              toReactType(f.ty.resolved, s"converted_field_$id", s"field_$id", w)
              val putMethodName = if (isContainer(f.ty.resolved)) "putArray" else "putMap"
              w.wl(s"""implementationsData.${putMethodName}("$fieldIdent", converted_field_$id);""")
            }
          })
          w.wl("this.implementationsData.putMap(currentInstanceUid, implementationsData);")
        }
      }

        //Field getters
        r.fields.foreach(f => {
          val id = r.fields.indexOf(f)
          val isFieldInterface = isExprInterface(f.ty.resolved)
          val isFieldRecord = isExprRecord(f.ty.resolved)
          val fieldIdent = idJava.field(f.ident)
          val suffix = fieldIdent.substring(0, 1).toUpperCase() + fieldIdent.substring(1)
          //Getter
          val getterName = s"get$suffix"
          val fieldDecl = generateParams(f) match {
            case Some((ident, decl)) => decl
            case None => ""
          }
          w.wl("@ReactMethod")
          w.wl(s"public void $getterName(ReadableMap currentInstance, Promise promise)").braced {
            val fieldTypeName = marshal.typename(f.ty.resolved)
            val javaFieldType = getRCTName(fieldTypeName)
            val reactFieldType = spec.reactNativeTypePrefix + javaFieldType
            w.wl("""String uid = currentInstance.getString("uid");""")
            w.wl("""if (uid.length() > 0)""").braced {
              //w.wl(s"${javaInterface} javaObj = this.javaObjects.get(uid);")
              if (isFieldInterface || isFieldRecord) {
                w.wl("if (!this.implementationsData.hasKey(uid))").braced {
                  w.wl("this.mapImplementationsData(currentInstance);")
                }
                w.wl("ReadableNativeMap data = this.implementationsData.getMap(uid);")
                val getMethodName = if (isContainer(f.ty.resolved)) "getArray" else "getMap"
                val fieldDataType = if (isContainer(f.ty.resolved)) "getArray" else "getMap"

                isContainer(f.ty.resolved) match {
                  case true => {
                    w.wl(s"""ReadableArray resultTmp = data.getArray("$fieldIdent");""")
                    w.wl("WritableNativeArray result = new WritableNativeArray();")
                    w.wl("for (int i = 0; i < resultTmp.size(); i++)").braced {
                      w.wl("WritableNativeMap result_elem = new WritableNativeMap();")
                      w.wl("result_elem.merge(resultTmp.getMap(i));")
                      w.wl("result.pushMap(result_elem);")
                    }
                  }
                  case _ => {
                    w.wl("WritableNativeMap result = new WritableNativeMap();")
                    w.wl(s"""result.merge(data.getMap("$fieldIdent"));""")
                  }
                }


                w.wl("promise.resolve(result);")
              } else {
                w.wl(s"${javaInterface} javaObj = this.javaObjects.get(uid);")
                val supportedFieldTypeName = javaFieldType match {
                  case "long" => "double"
                  case _ => javaFieldType
                }
                w.wl(s"$supportedFieldTypeName result = javaObj.$getterName();")
                toReactType(f.ty.resolved, "converted_result", "result", w)

                def resolvePromise(tm: MExpr) : Unit = {
                  tm.base match {
                    case MOptional => resolvePromise(tm.args.head)
                    case MList | MSet | MMap | MDate => w.wl(s"promise.resolve(converted_result);")
                    case _ => {
                      w.wl("WritableNativeMap resultMap = new WritableNativeMap();")
                      val localResult = tm.base match {
                        case MBinary => "converted_result"
                        case d: MDef =>
                          d.defType match {
                            case  DEnum => "result.ordinal()"
                            case _ => "result"
                          }
                        case _ => "result"
                      }
                      w.wl(s"""resultMap.${putMethod(tm)}("value", $localResult);""")
                      w.wl(s"promise.resolve(resultMap);")
                    }
                  }
                }
                resolvePromise(f.ty.resolved)
              }
            }
            w.wl("else").braced {
              w.wl(s"""promise.reject("Failed to call $self::$getterName", "First parameter of $self::$getterName should be an instance of $self");""")
            }
          }
          w.wl
        })
      }

    })
  }


  def writeAlignedReactNativeCall(w: IndentWriter, call: String, params: Seq[Field], end: String, f: Field => Option[(String, String)]) = {
    w.w(s"$call")
    params.foreach(p => {
      val isLast = params.indexOf(p) == params.length - 1
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
