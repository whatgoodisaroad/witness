# Addresses

There are three address spaces for a grid: cells, edges and vertices. The cell
address space is the natural address space by the row and column of the cell.
For example, if a grid is m x n cells, the top-left cell is at address (0, 0),
the top-right cell is at address (0, n - 1), the bottom right cell is at
address (m - 1, n - 1), and so on.

The vertex address space corresponds to the cell address to the south east of
the vertex. So, for example, in a grid with m x n cells, the vertex at the top
left is at address (0, 0) because the cell to the south east of it is cell
(0, 0). The special case is for vertices at the right or bottom of the grid
because these vertices do not have cells to their south easts. In these cases
the vertices have the address that a cell *would* have if there was one to
their south east. For example, in an m x n grid, the top right vertex has
address (0, n), the bottom right vertex has address (m, n) and so on.

The edge address space is made up of horizontal and vertical edges. For a
horizontal edge, it will have the address of the vertex at its left tip. For a
vertical edge, it will have the address of the vertex at its top tip.cs

    v(0,0) ----- e(0,0,h) ----- v(0,1) ----- ... ----- e(0,n-1,h) ----- v(0,n)
        |                         |                                       |
        |                         |                                       |
    e(0,0,v)     c(0,0)         e(0,1,v)     ...        c(0,n-s)       e(0,n,v)
        |                         |                                       |
        |                         |                                       |
    v(1,0) ----- e(1,0,h) ----- v(1,1) ----- ... ----- e(1,n-1,h) ------ v(1,n)
        |                         |                                       |
        .            .            .                         .             .
        .            .            .                         .             .
        .            .            .                         .             .
        |                         |                                       |
    v(m-1,0) -- e(m-1,0,h) -- v(m-2,1) ----- ... ---- e(m-1,n-1,h) ----- v(m-1,n)
        |                         |                                       |
        |                         |                                       |
    e(m-1,0,v)   c(m-1,1)      e(m-1,1,v)    ...       c(m-1,n-2)      e(m-1,n,v)
        |                         |                                       |
        |                         |                                       |
    v(m,0) ----- e(m,0,h) ----- v(m,1) ----- ... ----- e(m,n-1,h) ------ v(m,n)
