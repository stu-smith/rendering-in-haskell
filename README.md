# Rendering in Haskell

Experiments in 3d rendering in Haskell

Each experiment adds further features to the one before. The idea is to be able to diff consecutive experiments to see what has changed. (So, each experiment starts as a copy-paste of the previous).


### Experiment 01

*Basic image generation*

<img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment00.png" width="64" height="48" align="left" />

Ensuring that we can write an image. Red component increases along the x-axis, green component increases along the y-axis.

### Experiment 02

*Flat shading*

<img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment01.png" width="64" height="48" align="left" />

Rendering the Cornell box using flat shading. Shows basic ray-casting and nothing else.

### Experiment 02

*Diffuse shading*

<img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment02.png" width="64" height="48" align="left" />

Shows how to implement diffuse shading.

### Experiment 03

*Specular lightinh*

<img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment03.png" width="64" height="48" align="left" />

Shows the implementation of specular (shiny) lighting, which takes the location of the viewer into account.
