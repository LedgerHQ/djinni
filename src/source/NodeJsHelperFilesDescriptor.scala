package djinni

import djinni.generatorTools.Spec

class NodeJsHelperFilesDescriptor(spec: Spec) {
  val ObjectWrapperName = spec.nodeIdentStyle.ty(NodeJsHelperFilesDescriptor.objectWrapperLogicalName)
  val ObjectWrapperHeader = ObjectWrapperName + "." + spec.cppHeaderExt
}

object NodeJsHelperFilesDescriptor {
  protected val objectWrapperLogicalName = "ObjectWrapper"
}
