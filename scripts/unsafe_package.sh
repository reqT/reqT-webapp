#!/bin/sh

rm -f package.zip
rm -rf ./package/*

sbt dist

mkdir package

cp ./server/target/universal/server-0.1-SNAPSHOT.zip package/

cp ./scripts/start.sh package/

cd package

unzip server-0.1-SNAPSHOT.zip

cp ../reqT.jar .

mkdir server
cp -r ../server/app ./server/

mv server-0.1-SNAPSHOT/* .

rm -rf server-0.1-SNAPSHOT
rm -f server-0.1-SNAPSHOT.zip

cd ..

zip -r -9 package.zip package

rm -rf package
