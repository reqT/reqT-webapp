#!/bin/bash


### PWD: root/

rm -f ./package.zip
rm -rf ./package

sbt dist

mkdir package

cp ./server/target/universal/server-0.1-SNAPSHOT.zip package/

cd package

### PWD: root/package

echo "#!/bin/bash
rm -f RUNNING_PID
./bin/server" > start.sh

chmod +x start.sh

unzip server-0.1-SNAPSHOT.zip

cp ../reqT.jar .

mkdir server
cp -r ../server/app ./server/

mv server-0.1-SNAPSHOT/* .

rm -rf server-0.1-SNAPSHOT
rm -f server-0.1-SNAPSHOT.zip

cd ..

### PWD: root/

zip -r -9 package.zip package

rm -rf package
