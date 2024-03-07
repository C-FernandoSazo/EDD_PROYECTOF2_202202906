module listaImagen
    implicit none

    type, public :: NodoLimagen
        type(NodoLimagen), pointer :: siguiente => null()
    end type NodoLimagen

    type, public :: lista_imagen
        type(NodoLimagen), pointer :: head => null()
    end type lista_imagen
contains

    subroutine agregarImagen(lista)
        class(lista_imagen), intent(inout) :: lista
        type(NodoLimagen), pointer :: nuevoNodo, actual

        allocate(nuevoNodo)
        nuevoNodo%siguiente => null()

        if (.not. associated(lista%head)) then
            lista%head => nuevoNodo
        else 
            actual => lista%head
            do while(associated(actual%siguiente))
                actual => actual%siguiente
            end do 
                actual%siguiente => nuevoNodo
        end if

    end subroutine agregarImagen   

end module listaImagen


module listaAlbum
    use listaImagen
    implicit none

    type, public :: NodoAlbum
        type(lista_imagen) :: listImagenes
        type(NodoAlbum), pointer :: anterior => null()
        type(NodoAlbum), pointer :: siguiente => null()
    end type NodoAlbum

    type, public :: lista_album
        type(NodoAlbum), pointer :: head => null()
    end type lista_album

contains 
    subroutine agregarAlbum(lista)
        class(lista_album), intent(inout) :: lista
        type(NodoAlbum), pointer :: nuevoAlbum, actual

        allocate(nuevoAlbum)
        nuevoAlbum%anterior => null()
        nuevoAlbum%siguiente => null()

        if (.not. associated(lista%head)) then
            lista%head => nuevoAlbum
        else 
            actual => lista%head
            do while(associated(actual%siguiente))
                actual => actual%siguiente
            end do
                actual%siguiente => nuevoAlbum
                nuevoAlbum%anterior => actual
        end if
    
    end subroutine agregarAlbum

end module listaAlbum