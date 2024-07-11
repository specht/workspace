#!/bin/bash
cd docker/tic80 && docker build -t hs_code_tic80 . && cd ../..
mkdir -p temp/TIC-80
git clone --recursive https://github.com/specht/TIC-80.git temp/TIC-80
# cd temp/TIC-80 && git checkout v1.1.2837 && cd ../..
cd temp/TIC-80 && git checkout main && cd ../..
docker run --rm -it -v $(pwd)/temp/TIC-80:/src hs_code_tic80 bash -c "cd build && emcmake cmake -DBUILD_PRO=ON -DBUILD_WITH_JANET=ON -DBUILD_WITH_JS=ON -DBUILD_WITH_PYTHON=ON -DBUILD_WITH_SQUIRREL=ON -DBUILD_WITH_SCHEME=ON -DBUILD_WITH_WASM=ON -DBUILD_WITH_WREN=ON -DBUILD_WITH_FENNEL=ON -DBUILD_WITH_RUBY=ON -DBUILD_WITH_MOON=ON -DBUILD_WITH_ALL=ON -DBUILD_SDLGPU=ON -DCMAKE_BUILD_TYPE=MinSizeRel .. && cmake --build . --config MinSizeRel --parallel && js-beautify -r bin/tic80.js && ruby -e \"s = File.read('bin/tic80.js'); s.gsub!('?? =', '??='); s.gsub!('|| =', '||='); s.gsub!('tic80.wasm', 'tic80.wasm?' + Random.rand(1000000).to_s); File.open('bin/tic80.js', 'w') { |f| f.write(s) }\""
cp temp/TIC-80/build/bin/tic80* src/static/tic80/