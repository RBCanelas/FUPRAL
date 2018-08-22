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


    module shape_linkedList_mod

    use abstract_LinkedList_mod
    use types_mod

    private
    public :: shapeList

    type, extends(linkedlist) :: shapeList
    contains
    procedure :: print => printshapeList
    procedure :: printCurrent => printshapeListcurr
    end type shapeList

    contains

    subroutine printshapeListcurr(this)
    class(shapeList), intent(in) :: this
    class(*), pointer :: curr
    curr => this%currentValue() ! get current value
    select type(curr)
    type is (shape)
        call curr%print()
    class is (circle)
        call curr%print()
        class default
        stop '[printshapeList]: unexepected type of content: not a shape or derived type'
    end select
    end subroutine printshapeListcurr

    subroutine printshapeList(this)
    class(shapeList), intent(in) :: this
    class(*), pointer :: curr
    call this%reset()               ! reset list iterator
    do while(this%moreValues())     ! loop while there are values to print
        curr => this%currentValue() ! get current value
        select type(curr)
        type is (shape)
            call curr%print()
        class is (circle)
            call curr%print()
            class default
            stop '[printshapeList]: unexepected type of content: not a shape or derived type'
        end select
        call this%next()            ! increment the list iterator
    end do
    call this%reset()               ! reset list iterator
    end subroutine printshapeList

    end module shape_linkedList_mod
