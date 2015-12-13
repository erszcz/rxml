cd ../ && make
(test $? != 0 || grep -i warning rust_src/build.log) && cat rust_src/build.log
cd -
