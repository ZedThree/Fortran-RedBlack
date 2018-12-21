# Red-Black and Binary Search Trees in Fortran

This repo contains two modules with implementations of red-black and
binary search trees. I wrote them purely as a learning exercise and
not for actual use, so use at your own risk!

## Tests

pFUnit is bundled as a submodule. You may need to run the following
command to get it:

    git submodule update --init --recursive

Then build it like:

    mkdir externals/build_pfunit
    cd externals/build_pfunit
    cmake ../pFUnit/ -DMPI=NO -DOPENMP=NO \
      -DCMAKE_INSTALL_PREFIX=../install_pfunit
    make && make install

Then you can build and run the tests like:

    mkdir build
    cd build
    cmake .. -DCMAKE_PREFIX_PATH=$(pwd)/../externals/install_pfunit/
    make && ctest --verbose
