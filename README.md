# clondike

Clondike sets out to solve three deficits with the standard Klondike games available in Windows and Gnome:

* Command-line interface
* All games are guaranteed to be winnable
    * You are notified when the game is no longer winnable
* Cross platform compatible

Clondike is also intended as a platform for analyzing the game of Klondike.

Clondike is written in Clojure, hence the name :).

## Usage

    $ java -jar clondike-0.1.0-standalone.jar [-t]

## Options

* -t: force native terminal mode

## Planned Features

* Allow card moves (yeah, pretty major)
* Only present winnable games
* Notify player when game is won or unwinnable
    * Allow user to 'autocomplete' trvially winnable games
* More extensive use of Specter to simplify access to the game state
* Graceful handling of large tableau stacks on short vertical terminal windows
* Upgrade to Lanterna 3.0 when it becomes available
* Provide option for move 'hints' based on possible solutions
    * Full auto-play mode - watch the computer play
* Allow unlimited undo/redo
* Enable testing of heuristic strategies for gameplay
* Add machine learning capabilities
    * Watch the machine learning algorithm play
* ClojureScript UI

## License

Copyright © 2016 Stephen Rudolph

Distributed under the MIT License
