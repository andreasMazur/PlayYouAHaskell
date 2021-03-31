# PlayYouAHaskell

## About 'Play You a Haskell'

'Play You a Haskell' (PYaH) is an easy to use **framework** which allows to create 2D ASCII-games for the Bash-terminal in **Haskell**. For example, you can create game stories as multiple related 'chapters' with 'prgoram-holes', which shall be filled by the player in order to proceed into the next chapter. Thus, the usage of this framework is generally divided into two perspectives:

  * **The story-creator perspective**: Modeling the story by creating chapters with the help of the provided features from the framework
  * **The player perspective**: Using PYaH to access and play an existing story, fulfilling the story-given tasks

The framework shall encourage people to learn Haskell in a joyful way by creating the possibility to embed Haskell-programming tasks in games. Simultaneously, it shall be easy to create stories to motivate story-creators to program intersting storylines for the palyers. This is why 'Play You a Haskell' offers a game environment with basic physics and a variety of game features like *tripwires*, *an inventory* and more out of the box. 

## Installation and Starting PYaH

Currently, PYaH can only be used within the GHCi. Compiling it and executing the binaries currently causes some visual bugs. Furthermore, [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) is required, as PYaH was written as a Haskell Stack project.

You install PYaH by simply cloning this repository:

```
git clone https://github.com/andreasMazur/PlayYouAHaskell.git
```

Afterwards you start stack from the root directory of the project:

```
../PlayYouAHaskell> stack ghci
```

Then, you can hide the loaded Haskell modules within the GHCi-prompt:

```
ListOfManyModules> :set prompt ">"
```

Finally, one starts the game by executing the `main`-function:

```
> main
```

Now you are located in the main menu of PYaH, where you can select to load or start a new game.

## Story Creation and Available Features

PYaH is still under developement. *Soon*, an example story line will be available that shall illustrate how to create a new story and what features are currently available.
