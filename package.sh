#!/bin/bash

package_name="reqT"


### PWD: root/

rm -f ./$package_name.zip
rm -rf ./$package_name

sbt dist

mkdir $package_name

cp ./server/target/universal/server-0.1-SNAPSHOT.zip $package_name/

cp ./package-scripts/* $package_name/

cd $package_name

### PWD: root/package_name

unzip server-0.1-SNAPSHOT.zip

cp ../reqT.jar .

mkdir server
cp -r ../server/app ./server/

mv server-0.1-SNAPSHOT/* .

rm -rf server-0.1-SNAPSHOT
rm -f server-0.1-SNAPSHOT.zip

cd ..

### PWD: root/

zip -r -9 $package_name.zip $package_name

rm -rf $package_name
