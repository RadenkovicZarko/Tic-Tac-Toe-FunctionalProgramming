Tic Tac Toe in Haskell - A Functional Programming Approach

This repository contains a Haskell-based implementation of the classic game Tic Tac Toe, developed as part of our Functional Programming course. Emphasizing the principles of lambda calculus, recursion, monads, 
and functors, this project showcases the efficacy of functional programming in problem-solving within the domain of game development.

Project Overview
State Modeling: Utilizes complex data structures such as the Rose tree to represent game states.
Move Generation: Implements Haskell functions to calculate valid moves within the game.
Game Outcome Logic: Employs monads for state management, elegantly handling the game's flow and decision-making process.

Highlights
Advanced Functional Concepts: The codebase applies advanced functional programming techniques to ensure clarity, maintainability, and performance.
Higher-Order Functions: Demonstrates the power of Haskell in creating concise and expressive solutions for game logic and state manipulation.
Practical Application of Theory: Bridges theoretical concepts with real-world application, enhancing the understanding of functional paradigms.
This implementation not only serves as an educational tool for those learning Haskell or functional programming but also as an example of how to apply theoretical computer science concepts to tangible software projects. 
It's designed to encourage creative problem-solving and to advance knowledge in the fields of computing and software engineering.


------------------------------------------------
Getting Started
This project is written in Haskell and requires GHC (The Glasgow Haskell Compiler) for compiling and running. Follow the steps below to set up your environment and run the project.

Prerequisites
GHCup: This tool is used to manage GHC versions and related tools. Install GHCup following the instructions on the official GHCup website.
Setup
Install GHCup: Follow the installation guide for GHCup to set up GHC on your system if you haven't already.

Navigate to Project Directory: Open your terminal and change the directory to where your project is located. For example: cd path/to/your/project

Launch GHCi with Your Project: To start the GHC interactive environment with your project file, type: ghci projekat.hs

This command loads your Haskell file projekat.hs into GHCi, making the functions defined in the file available for use.

Running Functions
Once in GHCi, you can execute any function defined within projekat.hs. Usage examples for all functions are provided as comments within the file. Simply type the function name followed by its arguments to run it. For example: functionName arg1 arg2


Running the Main Program
To parse and process file.txt as specified by the main function in projekat.hs, simply type main in the GHCi prompt and press enter. This will execute the main function and display the results in the terminal: main


Ensure that file.txt is correctly formatted and located in the same directory as your project for the main function to work properly.
Note
Make sure to replace projekat.hs with the actual name of your Haskell file and file.txt with the name of the file you intend to parse, if they are different.
