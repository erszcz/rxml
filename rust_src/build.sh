cd ../ && make
#(test $? != 0 || grep -i warning rust_src/build.log) && cat rust_src/build.log
(test $? != 0) && cat rust_src/build.log
cd -
