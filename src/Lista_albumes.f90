module listaImagen
    implicit none

    type:: NodoLimagen
        integer :: id
        type(NodoLimagen), pointer :: siguiente => null()
    end type NodoLimagen

    type :: lista_imagen
        type(NodoLimagen), pointer :: head => null()
        contains
            procedure :: agregarImagen
            procedure :: mostrarImagenes
    end type lista_imagen
contains

    subroutine agregarImagen(lista, id)
        class(lista_imagen), intent(inout) :: lista
        integer, intent(in) :: id
        type(NodoLimagen), pointer :: nuevoNodo, actual

        allocate(nuevoNodo)
        nuevoNodo%id = id
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

    subroutine mostrarImagenes(lista)
        class(lista_imagen), intent(inout) :: lista
        type(NodoLimagen), pointer :: actual

        actual => lista%head
        do while(associated(actual))
            write(*,*) actual%id
            actual => actual%siguiente
        end do
    end subroutine mostrarImagenes

end module listaImagen


module listaAlbum
    use listaImagen
    implicit none

    type, public :: NodoAlbum
        character(len=10) :: id
        type(lista_imagen) :: listImagenes
        type(NodoAlbum), pointer :: anterior => null()
        type(NodoAlbum), pointer :: siguiente => null()
    end type NodoAlbum

    type, public :: lista_album
        type(NodoAlbum), pointer :: head => null()
        contains
            procedure :: agregarAlbum
            procedure :: addImagen
            procedure :: mostrarAlbum
    end type lista_album

contains 
    subroutine agregarAlbum(lista, id)
        class(lista_album), intent(inout) :: lista
        character(len=10), intent(in) :: id
        type(NodoAlbum), pointer :: nuevoAlbum, actual

        allocate(nuevoAlbum)
        nuevoAlbum%id = id
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

    subroutine addImagen(lista,id,img)
        class(lista_album), intent(inout) :: lista
        character(len=10), intent(in) :: id
        integer, intent(in) :: img
        type(NodoAlbum), pointer :: actual

        actual => lista%head
        do while(associated(actual))
            if (actual%id == id) then
                call actual%listImagenes%agregarImagen(img)
            end if
            actual => actual%siguiente
        end do
    end subroutine addImagen

    subroutine mostrarAlbum(lista)
        class(lista_album), intent(inout) :: lista
        type(NodoAlbum), pointer :: actual

        actual => lista%head
        do while(associated(actual))
            write (*,*) actual%id
            call actual%listImagenes%mostrarImagenes()
            actual => actual%siguiente
        end do
    end subroutine mostrarAlbum

end module listaAlbum