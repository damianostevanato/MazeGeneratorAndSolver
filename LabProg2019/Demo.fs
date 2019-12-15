module LabProg2019.Demo

open System
open Engine
open Gfx
open Maze

[< NoEquality; NoComparison >]
type state = {
    
    player : sprite
}

let main() = 
    let engine = new engine(150,31)
    engine.show_fps <- false 
    //maze doppio
    let maze = new maze(75,31)
    let (x,y) = maze.getStart()
    let player = engine.create_and_register_sprite (image.duplicate(image.rectangle (1,1, pixel.player Color.Red, pixel.player Color.Gray)),x+1, y, 2)
    let maze_doubled = engine.create_and_register_sprite (image.duplicate(maze.getMaze()),0, 0, 1)
   
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -2., 0.
            | 'd' -> 2., 0.
            | _   -> 0., 0.
        //blocks the player sprite if its going outside the maze 
        if(maze.legalMove(int(st.player.x+dx),int(st.player.y+dy),maze_doubled))then st.player.move_by(dx,dy)
        else st.player.move_by(0.,0.)
        if(maze.playerWin(int(st.player.x+dx),int(st.player.y+dy),maze_doubled)) then maze_doubled.draw_text("YOU WON!",(Console.WindowWidth/2),(Console.WindowHeight/2),Color.DarkRed)
         
        st, key.KeyChar = 'q'
    //maze non sdoppiato , utile per debug 
    (*let maze = new maze(75,31)
    let (x,y) = maze.getStart()
    let player = engine.create_and_register_sprite (image.rectangle (1,1, pixel.player Color.Red, pixel.player Color.Gray)),x, y, 2)
    let maze = engine.create_and_register_sprite (image.duplicate(maze.getMaze()),0, 0, 1)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.
        //blocks the player sprite if its going outside the maze 
        if(maze.legalMove(int(st.player.x+dx),int(st.player.y+dy),maze))then st.player.move_by(dx,dy)
        else st.player.move_by(0.,0.)
        st, key.KeyChar = 'q'*)
    let st0 = { 
        player = player
      }
  


    
   
    
    // start engine loop
    engine.loop_on_key my_update st0