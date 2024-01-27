make_art()
================
Bartłomiej Sadowski
2024-01-27

<style>
div {
text-align: justify}
</style>

<br>

# Make art in R

## Description

`make_art()` allows you to generate some random art in R. You can either
choose one out of four art types or fully rely on fate.

## Sourcing

To use `make_art()` simply source it from my github:

``` r
source("https://raw.githubusercontent.com/BartekSadowski/PIPS/main/functions_assignment_3_2R.R")
```

## Usage

``` r
make_art(seed, art_type = "random", outer_canvas = 1000, inner_canvas = outer_canvas)
```

## Arguments

<div>

|                |                                                                                                                                                                                                                                                                                                                                                                                                                              |
|----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `seed`         | You can optionally specify a random seed to make your art reproducible. By default no value of the random seed is prespecified. See `?set.seed` for more information.                                                                                                                                                                                                                                                        |
| `art_type`     | There are four kinds of art possible to generate. You can either prespecify what kind od art you want to generate by setting `art_type` to a relevant string or let the algorithm to choose it for you. More in details.                                                                                                                                                                                                     |
| `outer_canvas` | All art generated by `make_art()` comes in the format of square. You can, however, choose the length of the edge of this square (in pixels) by assigning a number to `outer_canvas`. By default `outer_canvas = 1000`                                                                                                                                                                                                        |
| `inner_canvas` | However, you do not have to use the whole generated square a.k.a. canvas for your art. The art can be generated centrally on a smaller part of the canvas. You can specify the length of the edge square within the whole canvas that is devoted to painting. The rest of the canvas will stay blank. Remember that `inner_canvas` has to be smaller that `outer_canvas`. By default `inner_canvas` equals to `outer_canvas` |
|                |                                                                                                                                                                                                                                                                                                                                                                                                                              |

</div>

## Details

<div>

`make_art()` produces four kinds of art: `"lines"`, `"angles"`,
`"squares"` and `"patches"`. If you want to generate any particular kind
of art, specify it in the `art_type` argument by assigning the name of a
kind of art that you are interested in. If you do not assign any vale or
you assign a different value then one of the four listed above,
`make_art()` will draw a type of art for you.

`"lines"`gives you an image of horizontal stripes of different hues from
a randomly generated gradient of colours.
`"angles" creates something similar to`“lines”\` but here some of the
stripes are vertical and they meet with horizontal one is a way that
resembles a right angle.

`"squares"` gives you an image created of smaller, colourful squares
organized in a grid. The size and number squares as well as their
colours are randomly determined. Moreover, half of the time, the
generated squares will be rotated by a random angle. To achieve the best
effect of this kind of art, it is recommended to set smaller
`inner_canvas` relative to `outer_canvas`.

`"patches"` gives you an image created by series of colourful patches of
the common pattern that are randomly located across the canvas. As the
location, their size, amount and colours are determined randomly.

Note that some kinds of art might render slightly longer then other.

</div>

## Examples

See several usage demonstrations of `make_art()`. Note that `make_art()`
displays images in a new window. The images here are embedded in the
file only for the illustration.

An art generated without any arguments specified:

``` r
make_art()
```

![](https://github.com/BartekSadowski/PIPS/blob/main/Pictures/example1.png?raw=true)

However, the code above always generates a different image. If you want
your art to be reproducible, set seed:

``` r
make_art(seed = 1234)
```

![](https://github.com/BartekSadowski/PIPS/blob/main/Pictures/example2.png?raw=true)

If you know to generate any image of the `"lines"` kind, specify
`art_type`:

``` r
make_art(art_type = "lines")
```

![](https://github.com/BartekSadowski/PIPS/blob/main/Pictures/example3.png?raw=true)

Suppose now, that you want the `"angles"` art type created on a canvas
with the egde length of 100 pixels:

``` r
make_art(art_type = "angles", outer_canvas = 100)
```

![](https://github.com/BartekSadowski/PIPS/blob/main/Pictures/example4.png?raw=true)

Now suppose, that you want to generate `"squares"` on the defaiult
`outer_canvas` but with `inner_canvas` set to 500 pixels:

``` r
make_art(art_type = "squares", inner_canvas = 500)
```

![](https://github.com/BartekSadowski/PIPS/blob/main/Pictures/example5.png?raw=true)

Last but not least, suppose that you want to generate `"patches"` on
`outer_canvas` set to 1100 pixels and `inner_canvas` set to 800 pixels:

``` r
make_art(art_type = "patches", outer_canvas = 1100, inner_canvas = 800)
```

![](https://github.com/BartekSadowski/PIPS/blob/main/Pictures/example6.png?raw=true)

## The end!

Now you now everything about `make_art()` function and you can become an
Rtist! Happy painting! :)

<br> <br>