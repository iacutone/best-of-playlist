This repository contains a collection of my favorite songs


### Run Elm 0.18 with Docker
`docker pull codesimple/elm:0.18`
`docker run -it --rm -v "$(pwd):/code" -w "/code" -e "HOME=/tmp" -u $UID:$GID -p 8000:8000 codesimple/elm:0.18 make Main.elm --output=app.js`
