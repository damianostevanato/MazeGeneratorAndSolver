(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze
open System
open External
open Gfx
open System.Threading

type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    static member maze_end =pixel.create (Config.wall_pixel_char, Color.Magenta)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled Color.Black
   
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.wall


// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze (w, h) as this =
    let maze = image(w,h)
    let mutable movements_stack = []  
    let mutable movements_stack_copy = [] 
    let mutable end_maze = (0,0)
    let mutable random = 0  
    let mutable start_point = (0,0)
    let mutable end_point = (0,0) 
    // check if the given coordinates are inside the border of the maze 
    let isInsideBorders(x:int,y:int):bool = 
        (x>0 && x<w-1) && (y>0 && y<h-1)
    // creates the possible movements from a certain coordinates
    let movements(x,y) = 
        [(x,y-2);(x-2,y);(x,y+2);(x+2,y)]

    let choose(movements :(int*int) list) =
        movements.Item(rnd_int 0 (movements.Length-1))

    //removes from the movements list the movements which are incorrect 
    (*let rec check_movements(movements :(int*int) list)=
        match movements with 
        |[] -> [] 
        |(x,y)::xs ->if(isInsideBorders(x,y) && (maze.get(x,y) = pixel.wall) && ((List.contains((x,y))movements_stack)=false)) then (x,y)::check_movements(xs) else check_movements(xs)*)
    //
    let rec check_movements (movements :(int*int) list , filtered: (int*int) list)=
        if(movements.Length <> 0 ) then if(isInsideBorders(List.head(movements)) && (maze.get(List.head(movements))=pixel.wall) && (List.contains(List.head(movements))movements_stack)=false) then check_movements (List.filter((<>) (List.head(movements)))movements ,List.head(movements)::filtered)
                                        else check_movements (List.filter((<>) (List.head(movements)))movements,filtered)
        else filtered
    // removes the wall in certain x,y of the maze 
    let removeWall(x:int,y:int,sx:int,sy:int)=
        if((x,y) = (sx,sy-2)) then   maze.plot(sx,sy-1,pixel.path) 
        if((x,y) = (sx-2,sy)) then   maze.plot(sx-1,sy,pixel.path)                           
        if((x,y) = (sx,sy+2)) then   maze.plot(sx,sy+1,pixel.path)                           
        if((x,y) = (sx+2,sy)) then   maze.plot(sx+1,sy,pixel.path)                         
        movements_stack <- (x,y)::movements_stack
        movements_stack_copy <-(x,y)::movements_stack_copy
        maze.plot(x,y,pixel.path)
       

    

    let rec generateMaze(x:int,y:int,first_cicle:bool) = 
        
        if(first_cicle)then 
            start_point <- (x,y)
            
            removeWall(x,y,x,y)
            generateMaze(x,y,false)
        else 
            let moves = movements(x,y)
            let possible_moves = check_movements(moves,[])
            if(possible_moves = [] && movements_stack_copy = []) then ()
            else 
                if(possible_moves = [] ) then movements_stack_copy <- List.filter((<>)(List.head(movements_stack_copy)))movements_stack_copy
                                              if(movements_stack_copy = [] )then () 
                                              else 
                                                   let x1,y1 = List.head(movements_stack_copy)
                                                   generateMaze(x1,y1,false)
                else let mov_x,mov_y = choose(possible_moves)
                     removeWall(mov_x,mov_y,x,y)
                     generateMaze(mov_x,mov_y,false)
   
    
    do this.generate
    // TODO: start with implementing the generation
    // TODO: do not forget to call the generation function in your object initializer
    member this.setEnd() = 
        random <- (rnd_int 0 2)
        if(random = 0)then maze.plot(w-1,1,pixel.maze_end) 
                           end_point <- (w-1,1)
        else if(random = 1)then maze.plot(w-2,h-1,pixel.maze_end)
                                end_point <- (w-2,h-1)
             else if(random = 2)then maze.plot(1,h-1,pixel.maze_end)
                                     end_point <- (1,h-1)
                                    
    
    member this.getMaze()=
           this.setEnd()
           maze
    member this.getStart() =
          start_point
    member this.getEnd() = 
          end_maze
    member this.legalMove(x:int,y:int,maze:image)=
        (maze.get(x,y)=pixel.path)
    member this.playerWin(x:int,y:int,maze:image) =
        (maze.get(x,y)=pixel.maze_end)
        
    member private __.generate = 
        maze.flood_fill(0,0,pixel.wall)
        let r_x,r_y = (1, 1) // watchout for borders
        generateMaze(r_x,r_y,true)
        
       
        
        
        


    