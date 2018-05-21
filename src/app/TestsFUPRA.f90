    !MARETEC - Research Centre for Marine, Environment and Technology
    !Copyright (C) 2018  Ricardo Birjukovs Canelas
    !
    !This program is free software: you can redistribute it and/or modify
    !it under the terms of the GNU General Public License as published by
    !the Free Software Foundation, either version 3 of the License, or
    !(at your option) any later version.
    !
    !This program is distributed in the hope that it will be useful,
    !but WITHOUT ANY WARRANTY; without even the implied warranty of
    !MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !GNU General Public License for more details.
    !
    !You should have received a copy of the GNU General Public License
    !along with this program.  If not, see <https://www.gnu.org/licenses/>.


    program TestsFUPRA
    
    use shape_array_mod
    use types_mod

    implicit none

    integer i    
    type(shapeArray)  :: shp_array
    integer values(10)
    type(shape) :: square
    type(circle) :: round
    integer :: test_spots

    square%id = 16
    square%filled = .true.

    !shapeArray Tests
    test_spots = 5
    call shp_array%init(test_spots)
    do i=1, test_spots
        square%id = i
        call shp_array%put(i, square)
    enddo

    print*, "From our amazing container array!"
    call shp_array%printArray()

    round%id = 517
    round%radius = 1.78
    round%filled = .false.

    call shp_array%put(1, round)
    print*, "Putting back a value on our amazing container array!"
    call shp_array%printArray()

    test_spots=3
    call shp_array%resize(test_spots)
    do i=6, test_spots
        square%id = i
        call shp_array%put(i, square)
    enddo

    print*, "From our amazing resized container array!"
    call shp_array%printArray()
    call shp_array%printElement(1)
    call shp_array%printElement(5)

    end program TestsFUPRA
