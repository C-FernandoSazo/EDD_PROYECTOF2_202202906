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
            procedure :: eliminarImagen
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

    subroutine eliminarImagen(lista, id)
        class(lista_imagen), intent(inout) :: lista
        integer, intent(in) :: id
        type(NodoLimagen), pointer :: actual, anterior 
    
        actual => lista%head
    
        if (.not. associated(actual)) then
            return
        end if
    
        ! Si el nodo a eliminar es la cabeza de la lista
        if (actual%id == id) then
            lista%head => actual%siguiente
            nullify(actual%siguiente)
            deallocate(actual)
            print *,"Imagen eliminada del album"
            return
        end if
    
        ! Buscar el nodo a eliminar, manteniendo un seguimiento del nodo anterior
        do while(associated(actual%siguiente))
            anterior => actual
            actual => actual%siguiente
            if (actual%id == id) then
                anterior%siguiente => actual%siguiente
                nullify(actual%siguiente)
                deallocate(actual)
                print *,"Imagen eliminada del album"
                return
            end if
        end do
    
    end subroutine eliminarImagen    

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