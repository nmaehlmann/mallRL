# mallRL

mallRL is a grocery shopping roguelike developed for the 7drl game jam.

![a gif showing mallrl](/screenshots/indoor.png)

As the game was developed under strict time constraints the code is neither well organized nor high quality, but might still be interesting for people looking to develop a game in haskell.

The jam version is tagged as 7drl-release and was developed from monday 02.03.2020 to sunday 08.03.2020. The week before the jam I took 2 days to prepare the technical side, as I was developing the game from scratch without an engine using SDL2 and an ECS library. After my preparation I had the ECS library included, was able to read keyboard inputs and render tiles. On monday I started the jam with a tile that could be moved on screen.

## Building on Windows
### Install SDL2 via stack & pacman:

```stack exec -- pacman -Syu```

```stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2```

### Build and install via stack:

```stack install```

```stack run```
