# mall
## Building on Windows
### Install SDL2 and SDL2_images via stack & pacman:

```stack exec -- pacman -Syu```

```stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2```

```stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2_image```

### Build and install via stack:

```stack install```

```stack run```
