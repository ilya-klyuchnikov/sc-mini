# Minimal Supercompiler

This is a minimal supercompiler (`sc-mini`).
The goal is to stress the main features of supercompilation for a working functional programmer.
Its design is an attempt to illustrate the following formula:

    Supercompiler = Driving
                  + Positive information propagation
                  + Folding
                  + Simplification
                  + Generalization

However, it turns out to be not so easy task. So here we illustrate this formula step-by-step:

1. Prototype = Driving + Generalization + Folding
2. Deforester = Prototype + Simplification of graph of configurations
3. Supercompiler = Deforester + Positive Information Propagation

In order to catch the idea of this supercompiler, just look into the following modules into the following order:

1. Prototype
2. Deforester
3. Supercompiler

## Detailed tutorial

There is a tutorial (64 pages) explaining in detail every aspect of `sc-mini`.

*Ilya Klyuchnikov and Dimitur Krustev*.
[Supercompilation: Ideas and Methods (+appendix)](https://themonadreader.files.wordpress.com/2014/04/super-final.pdf).
The Monad Reader 23 (2014)
