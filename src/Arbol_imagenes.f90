module Arbol_Imagenes
    use Arbol_Capas
    implicit none

    type :: nodoImagen
        integer :: id
        integer :: altura = 0
        type(ArbolCapas) :: arbolCapas
        type(nodoImagen), pointer :: siguiente => null()
    end type nodoImagen

    type :: ArbolImagenes
        type(nodoImagen), pointer :: head => null()
    end type ArbolImagenes

contains

    subroutine add()

    end subroutine add

end module Arbol_Imagenes