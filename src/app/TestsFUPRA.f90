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
  use shape_linkedList_mod
  use types_mod

  implicit none

  integer i
  type(shapeArray)  :: shp_array, testarray, testarray2
  type(shapeList) :: shp_list, testlist1, testlist2
  integer :: values(10)
  type(shape) :: square
  type(circle) :: round
  integer :: test_spots
  class(*), pointer :: something
  
  real(8) :: start1, finish1, start2, finish2, start3, finish3
  integer :: size1, size2, size3
  integer, allocatable, dimension(:) :: normIntArr
  
  

  square%id = 16
  square%filled = .true.

  call square%print()

  round%id = 517
  round%filled = .false.
  round%radius = 1.78
  
  call round%print()

  !shapeArray Tests
  test_spots = 5
  call shp_array%init(test_spots)
  do i=1, test_spots
    square%id = i
    call shp_array%put(i, square)
    call shp_list%add(square)
  enddo

  print*, "Printing our amazing container array!"
  call shp_array%printArray()

  round%id = 517
  round%radius = 1.78
  round%filled = .false.

  print*, "Replacing a value on our amazing container array with something different!"
  call shp_array%put(1, round)
  call shp_list%add(round)
  print*, "Printing our amazing container array!"
  call shp_array%printArray()

  test_spots=3
  call shp_array%resize(test_spots)
  do i=3, test_spots
    square%id = i
    call shp_array%put(i, square)
    !square = shp_array%get(2)
  enddo

  print*, "Printing again our amazing resized container array!"
  call shp_array%printArray()

  print*, "Getting something out of our amazing container array and printing it!"
  something => shp_array%get(2)
  select type(something)
  type is (shape)
    something%id = 666 !changing a value directly on the array
    call something%print()
    class default
    stop 'Unexepected type of content: not a shape'
  end select

  print*, "Printing some elements from our amazing container array!"
  call shp_array%printElement(1)
  call shp_array%printElement(2)
  call shp_array%printElement(3)
  
  call testarray%init(1)
  print*, 'testarray is ', testarray%getLength(), ' elements long'  
  call testarray%resize(testarray%getLength()+10)  
  print*, 'testarray is ', testarray%getLength(), ' elements long'



  print*, '----------------------Linked List-----------------------'

  call shp_list%print()
  
  print*, '-------------------Performance tests--------------------'
  test_spots = 10000000
  print*, 'Allocating and assigning ', test_spots, ' integers'
  print*, 'Simple integer array'
  
  
  
  call cpu_time(start1)
    allocate(normIntArr(test_spots))
    normIntArr = 10
  call cpu_time(finish1)
  !size1 = sizeof(normIntArr)
  
  print*, 'Polymorphic array'
  
  call cpu_time(start2)
    call testarray2%init(test_spots, initvalue = 10)
  call cpu_time(finish2)
  !size3 = testarray2%getMemSize()

  print*, 'Polymorphic Linked List'
  
  call cpu_time(start3)
  do i=1, test_spots
    call testlist2%add(10)
  enddo
  call cpu_time(finish3)
  
  print '(" Time for normal array = ",f15.6," seconds.")', finish1-start1
  print '(" Time for poly array   = ",f15.6," seconds.")', finish2-start2
  print '(" Time for poly List   = ",f15.6," seconds.")', finish3-start3
  !print '(" Size of normal array  = ",f15.6," mb.")', size1*1E-6
  !print '(" Size of poly array    = ",f15.6," mb.")', size3*1E-6

  

end program TestsFUPRA
