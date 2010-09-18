#!/bin/sh

#original from GMP.framework/build-framework.sh
# This is the shell script used to create this framework
# from the sources available from the GMP web page at
# http://gmplib.org
# To build your own, copy this file next to a GMP source tree,
# update the SrcDir variable if necessary and execute
# sh build-framework.sh

rm -rf build
mkdir -p build
BuildDir=`pwd`/build

SrcDir=`pwd`/gmp-5.0.1
FrameworkName=GMP
FrameworkVersion=A
LibraryName=libgmp.dylib
ExtraThings="$BuildDir/StagingArea32/info $SrcDir/README $SrcDir/COPYING.LIB $SrcDir/ChangeLog build-framework.sh"


pushd $BuildDir || exit 1

make distclean
CFLAGS='-arch i386' $SrcDir/configure --enable-cxx --host=none-apple-darwin --disable-static --enable-shared --prefix=$BuildDir/StagingArea32 || exit 1
make -j6 CCLD='gcc -Wc,-arch -Wc,i386' || exit 1
make install || exit 1

make distclean
CFLAGS='-arch x86_64' $SrcDir/configure --enable-cxx --host=none-apple-darwin --disable-static --enable-shared --prefix=$BuildDir/StagingArea64 || exit 1
make -j6 CCLD='gcc -Wc,-arch -Wc,x86_64' || exit 1
make install || exit 1

popd


rm -rf $FrameworkName.framework

FWVDir=$FrameworkName.framework/Versions/$FrameworkVersion
mkdir -p $FWVDir

cp -R $BuildDir/StagingArea32/include $FWVDir/Headers
# cp $SrcDir/StagingArea/lib/$LibraryName $FWVDir/$FrameworkName
lipo -arch i386 $BuildDir/StagingArea32/lib/$LibraryName -arch x86_64 $BuildDir/StagingArea64/lib/$LibraryName -create -output $FWVDir/$FrameworkName

install_name_tool -id $FWVDir/$FrameworkName $FWVDir/$FrameworkName

ln -sf Versions/$FrameworkVersion/$FrameworkName $FrameworkName.framework/$FrameworkName
ln -sf Versions/$FrameworkVersion/Headers $FrameworkName.framework/Headers

for i in $ExtraThings; do
    cp -R $i $FrameworkName.framework/
done

rm -rf $BuildDir

echo "Framework $FrameworkName.framework created."
