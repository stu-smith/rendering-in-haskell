# Rendering in Haskell

Experiments in 3d rendering in Haskell

Each experiment adds further features to the one before. The idea is to be able to diff consecutive experiments to see what has changed. (So, each experiment starts as a copy-paste of the previous).

| Image | Title | Explanation |
| ----- | ----- | ----------- |
| <img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment00.png" width="128" height="96" align="left" /> | 00 - Basic Image Generation | Ensuring that we can write an image. Red component increases along the x-axis, green component increases along the y-axis. |
| <img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment01.png" width="128" height="96" align="left" /> | 01 - Flat Shading | Rendering the Cornell box using flat shading. Shows basic ray-casting and nothing else. |
| <img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment02.png" width="128" height="96" align="left" /> | 02 - Diffuse Lighting | Shows how to implement diffuse shading. |
| <img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment03.png" width="128" height="96" align="left" /> | 03 - Specular Lighting | Shows the implementation of specular (shiny) lighting, which takes the location of the viewer into account. |
| <img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment04.png" width="128" height="96" align="left" /> | 04 - Reflections | Implements reflective and semi-reflective surfaces, rendered recursively. |
| <img src="https://raw.githubusercontent.com/stu-smith/rendering-in-haskell/master/output/experiment05.png" width="128" height="96" align="left" /> | 05 - Shadows | Implements hard shadows. |
