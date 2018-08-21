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

    module types_mod

    private
    public :: shape, circle

    type shape
        integer :: id
        logical :: filled
    contains
    procedure :: print=>printShape  ! print shape
    end type shape

    type, extends(shape) :: circle
        real :: radius
    contains
    procedure :: print=>printCircle  ! print shape
    end type circle

    contains

    subroutine printShape(this)
    class(shape), intent(in) :: this
    print*, 'id = ', this%id
    print*, 'is filled = ', this%filled
    end subroutine printShape

    subroutine printCircle(this)
    class(circle), intent(in) :: this
    print*, 'id = ', this%id
    print*, 'radius = ', this%radius
    print*, 'is filled = ', this%filled
    end subroutine printCircle

    end module types_mod
