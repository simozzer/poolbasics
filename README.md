# poolbasics
An investigation into implementing the basic physics for a pool/billiard game

Currently a very rough work in progress

Developed using Free Pascal and Lazarus


Planned next tasks:

Improved pocket detection.

Work out collision detection for 2 moving circles.


I've tried to attempt this using maths and physics. Rather than moving an item a small bit for each frame produced I'm aiming for a 'predictive' system which should forecast where each ball will end up.

For each ball it should be possible to forecast where it will end up, how long it will take, and the distance travelled.

It should also be able to predict the point in time, and position of the earliest collision within the system (either a collision with another ball, a pocket, or a cushion).

Once the earliest collision has been detected the algoritm should restart with the current state of play, and continue to operate until all objects have stopped moving.

This will create a data structure which can be queried for any point in time, allowing the window refresh to be hooked in to produce 'real time' animation.





