# balls

This is a simple physics simulation for balls

# Controls

Use your mouse to place balls; left clicking creates a stationary ball. right-click and drag creates a moving ball.

you can change the size of the ball you place with `q` (decrement) and `e` (increment) or with the scroll wheel.

You can pause/unpause the simulation with `p`

# options

## Presets

There are three preset environments which capture the most interesting behaviour of the system. You can choose between them with the `-p` flag. The options are `Ballistics`, `IdealGas` and `Orbital`. Ballistics is the default.

Ballistics has downwards global gravity and energy loss in collisions with the floor. There are no inter-particle forces.

Ideal gas has no global gravity or inter-particle forces. All Collisions (including those with the walls) are totally elastic; velocity-Colour is on.

Orbital has no bounds, but there are inter-particle forces - so you can use it to simulate orbits. This can be quite fiddly - pausing can be helpful.

## Custom

You can also provide your own custom settings; However, for reasons to do with our option parsing (its an applicative) you can't use both presets and custom options both at once - so you need to fully specify what you want. Sorry!

### Colour

Turn \on this switch (`-c`) and the balls colour will be determined by their velocity. Slow (or 'cold') balls will be more blue, and fast (or 'hot') balls will be more red. This is on for IdealGas and off for Ballistics

### Decay

The decay constant controls how much energy is lost when a ball collides with the floor; you can set it with `-d <float>`. The balls outgoing velocity is multiplied by this number; so `d=1` indicates no loss, and `d=0` indicates total loss. For an idea of scale, the ballistics simulation uses `d=0.8`.

### g

The force due to gravity, controlled with `-g <float>`. IdealGas uses `g=0`, whereas Ballistics uses `g=9.81`. N.B. that `g<0` will apply upwards gravity

### Boundaries

Boundaries are the edges of the box; by default we set this to ThreeSides - so a box with an open top. You can specify a preset (either `Box` or `ThreeSides`) but you can also give a pattern.

A pattern consists of four characters; `x` indicates a boundary, anything else indicates open.
We use the CSS shorthand (top, right, bottom, left); so `xxxx` is a box and `_xxx` has an open top.
no top or bottom would be `_x_x`.

# References

The 2-d particle collision logic is stolen straight from [this paper](https://www.imada.sdu.dk/~rolf/Edu/DM815/E10/2dcollisions.pdf).
