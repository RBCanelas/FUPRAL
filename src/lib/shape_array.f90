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


    module shape_array_mod

    use abstract_container_array_mod
    use types_mod

    private
    public :: shapeArray

    type, extends(container_array) :: shapeArray
    contains
    procedure :: printArray => printshapeArray
    procedure :: printElement => printshapeElement
    end type shapeArray

    contains

    subroutine printshapeArray(this)
    class(shapeArray), intent(in) :: this
    class(*), pointer :: curr
    integer :: i
    do i=1, this%getLength()
        curr => this%get(i)
        select type(curr)
        type is (shape)
            call curr%print()
        class is (circle)
            call curr%print()
            class default
            stop '[printshapeArray]: unexepected type of content: not a shape or derived type'
        end select
    end do
    end subroutine printshapeArray

    subroutine printshapeElement(this,index)
    class(shapeArray), intent(in) :: this
    integer, intent(in) :: index
    class(*), pointer :: curr
    if (index .le. this%getLength()) then
        curr => this%get(index)
        select type(curr)
        type is (shape)
            call curr%print()
        class is (circle)
            call curr%print()
            class default
            stop '[printshapeElement]: unexepected type of content, not a shape or derived type'
        end select
    else
        stop '[printshapeElement]: index out of bounds'
    endif
    end subroutine printshapeElement

    end module shape_array_mod
