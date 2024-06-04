# poolbasics
An investigation into implementing the basic physics for a pool/billiard game

Currently a very rough work in progress

Developed using Free Pascal and Lazarus


Planned next tasks:

Improved pocket detection.

Work out collision detection for 2 moving circles.


#Thoughts

Detecting collision between 2 decelerating circles is quite complex.
Reducing the problem may help:
- 1st check if the direction of the movemnt vectors could cause a possible collision (with no time/acceleration involved).
- If they could collide then work out for 1 particle (particle A) when the collision could possibly occur when velocity and deceleration are included.
- - Calculate the minimum and maximum times when a collision could have happened.
  - Work out if Particle B can travel far enough to reach the collision point between these times.
    = IF it can then find the Min/Max times for this type of collision
    - Do a load more thinking before coding more.
    - 
    



