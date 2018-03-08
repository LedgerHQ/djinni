#! /usr/bin/env bash
set -eu
shopt -s nullglob

# Locate the script file.  Cross symlinks if necessary.
loc="$0"
while [ -h "$loc" ]; do
    ls=`ls -ld "$loc"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "$link" : '/.*' > /dev/null; then
        loc="$link"  # Absolute link
    else
        loc="`dirname "$loc"`/$link"  # Relative link
    fi
done
base_dir=$(cd "`dirname "$loc"`" && pwd)

temp_out="$base_dir/djinni-output-temp"

in="$base_dir/example.djinni"

cpp_out="$base_dir/generated-src/cpp"
jni_out="$base_dir/generated-src/jni"
objc_out="$base_dir/generated-src/objc"
nodejs_out="$base_dir/generated-src/nodejs"
java_out="$base_dir/generated-src/java/com/dropbox/textsort"
react_out="$base_dir/generated-src/react-native"

java_package="com.dropbox.textsort"

gen_stamp="$temp_out/gen.stamp"

if [ $# -eq 0 ]; then
    # Normal build.
    true
elif [ $# -eq 1 ]; then
    command="$1"; shift
    if [ "$command" != "clean" ]; then
        echo "Unexpected argument: \"$command\"." 1>&2
        exit 1
    fi
    for dir in "$temp_out" "$cpp_out" "$jni_out" "$java_out"; do
        if [ -e "$dir" ]; then
            echo "Deleting \"$dir\"..."
            rm -r "$dir"
        fi
    done
    exit
fi

# Build djinni
"$base_dir/../src/build"

[ ! -e "$temp_out" ] || rm -r "$temp_out"
"$base_dir/../src/run-assume-built" \
    --java-out "$temp_out/java" \
    --java-package $java_package \
    --java-class-access-modifier "package" \
    --java-generate-interfaces true \
    --java-nullable-annotation "javax.annotation.CheckForNull" \
    --java-nonnull-annotation "javax.annotation.Nonnull" \
    --ident-java-field mFooBar \
    \
    --cpp-out "$temp_out/cpp" \
    --cpp-namespace textsort \
    --ident-cpp-enum-type foo_bar \
    \
    --jni-out "$temp_out/jni" \
    --ident-jni-class NativeFooBar \
    --ident-jni-file NativeFooBar \
    \
    --objc-out "$temp_out/objc" \
    --objcpp-out "$temp_out/objc" \
    --objc-type-prefix TXS \
    --objc-swift-bridging-header "TextSort-Bridging-Header" \
    \
    --node-out "$temp_out/nodejs" \
    --node-type-prefix NJS \
    --node-include-cpp "../cpp" \
    --node-package ledgerapp_nodejs \
    --react-native-objc-out "$temp_out/react-native" \
    --react-native-type-prefix RCT \
    --react-include-objc "../objc" \
    --react-include-objc-impl "../../handwritten-src/objc" \
    --react-native-objc-impl-suffix "Impl" \
    \
    --idl "$in" \
    --idl-include-path "../../" \
    --idl-include-path "path/to/idl"

# Copy changes from "$temp_output" to final dir.

mirror() {
    local prefix="$1" ; shift
    local src="$1" ; shift
    local dest="$1" ; shift
    mkdir -p "$dest"
    rsync -r --delete --checksum --itemize-changes "$src"/ "$dest" | sed "s/^/[$prefix]/"
}

echo "Copying generated code to final directories..."
mirror "cpp" "$temp_out/cpp" "$cpp_out"
mirror "java" "$temp_out/java" "$java_out"
mirror "jni" "$temp_out/jni" "$jni_out"
mirror "objc" "$temp_out/objc" "$objc_out"
mirror "nodejs" "$temp_out/nodejs" "$nodejs_out"
mirror "react-native" "$temp_out/react-native" "$react_out"

date > "$gen_stamp"

echo "djinni completed."
