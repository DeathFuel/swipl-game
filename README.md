# swipl-game
A small top-down action adventure game written in SWI-Prolog. Requires version 9.x.x and support for Unicode characters.
Note: some terminal emulators may display the characters improperly. In my testing, Konsole worked best, while Alacritty misplaced or cut off certain wide characters - though YMMV. 

Made for educational purposes; published as part of a gentlemanly agreement.
## Running
Run with SWI-Prolog 9.x.x and load the game with the `l/0` predicate:
```bash
$ swipl game.prolog
```
```prolog
?- l.
```
## Game map (spoilers 🙃)
<picture>
  <a href="https://raw.githubusercontent.com/DeathFuel/swipl-game/main/map.png">
    <img width="720px" alt="Game map. Click to view full size" src="https://raw.githubusercontent.com/DeathFuel/swipl-game/main/map.png">
  </a>
</picture>

## Warning
The game can be unstable at times. Don't run this if you're sensitive to flickering lights.
