# poolbasics
An investigation into implementing the basic physics for a pool/billiard game

Currently a very rough work in progress

Developed using Free Pascal and Lazarus


Planned next tasks:

Fix basic collision detection loop: Ensure that collisions are ordered correctly for objects which start moving after the initial puck movement (as a result of collision)

For each moving object:
  Calculate which object has a collision first.
    Then plot the trajectory of any objects involved and start calculation again.
      Repeat until all objects are stationary


