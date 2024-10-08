module listaAlbumss
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
            procedure :: deleteImagen
            procedure :: mostrarAlbum
            procedure :: graficar_albums
            procedure :: conteoAlbumImg
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

    subroutine deleteImagen(lista,img)
        class(lista_album), intent(inout) :: lista
        integer, intent(in) :: img
        type(NodoAlbum), pointer :: actual
        actual => lista%head
        do while(associated(actual))
            call actual%listImagenes%eliminarImagen(img)
            actual => actual%siguiente
        end do
    end subroutine

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

    subroutine graficar_albums(lista)
        class(lista_album), intent(in) :: lista
        character(len=15) :: filename = "lista_album"
        type(NodoAlbum), pointer :: nodoActual
        type(NodoLimagen), pointer :: nodoImgActual
        integer :: fileUnit, iostat, contador, contadorImg
        character(len=256) :: dotPath, pngPath

        dotPath = 'dot/' // trim(filename) // '.dot'
        pngPath = 'img/' // trim(adjustl(filename))
    
        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if
    
        print *,"Generando Lista de Albums..."
        write(fileUnit, *) "digraph albums {"
        write(fileUnit, *) "    rankdir=LR;"
        write(fileUnit, *) "    node [shape=record];"
    
        if (.not. associated(lista%head)) then
            write(fileUnit,*) '}'
            close(fileUnit)
            return
        end if
    
        nodoActual => lista%head
        contador = 0
        do while(associated(nodoActual))
            contador = contador + 1
            write(fileUnit, *) '"Node', contador, '" [label="', trim(nodoActual%id), '"];'

            nodoImgActual => nodoActual%listImagenes%head
            contadorImg = 0
            do while(associated(nodoImgActual))
                contadorImg = contadorImg + 1
                write(fileUnit, *) '"Node', contador, 'Img', contadorImg, '" [label=" Imagen: ', nodoImgActual%id, '"];'
                if (contadorImg == 1) then
                    write(fileUnit, *) '"Node', contador, '" -> "Node', contador, 'Img', contadorImg, '" [color=green];'
                end if
                if (associated(nodoImgActual%siguiente)) then
                    write(fileUnit, *) '"Node', contador, 'Img', contadorImg, '" -> "Node', contador, 'Img', & 
                    contadorImg+1, '" [color=green];'
                end if
                nodoImgActual => nodoImgActual%siguiente
            end do

            ! Conexión al siguiente nodo si no es el mismo
            if (associated(nodoActual%siguiente)) then
                write(fileUnit,*) '    "Node', contador, '" -> "Node', contador+1, '" [color=blue];'
                write(fileUnit,*) '    "Node', contador+1, '" -> "Node', contador, '" [color=red];'
            end if
    
            nodoActual => nodoActual%siguiente
        end do
    
        write(fileUnit, *) "}"    
        close(fileUnit)
        call system('dot -Tpng ' // trim(dotPath) // ' -o ' // trim(adjustl(pngPath)) // '.png')   
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
        print *,"Lista de Albums graficada con exito"
        call system('start ' // trim(adjustl(pngPath)) // '.png')
    end subroutine graficar_albums

    subroutine conteoAlbumImg(lista)
        class(lista_album), intent(in) :: lista
        type(NodoAlbum), pointer :: actual
        type(NodoLimagen), pointer :: imgActual
        integer :: contAlbum = 0, contImg = 0

        actual => lista%head
        do while(associated(actual))
            contAlbum = contAlbum + 1
                imgActual => actual%listImagenes%head
                do while(associated(imgActual))
                    contImg =  contImg + 1
                    imgActual => imgActual%siguiente
                end do
            actual => actual%siguiente
        end do

        write(*,'(A,I0)') 'El total de albumes del cliente es: ', contAlbum
        write(*,'(A,I0)') 'El total de de imagenes en los albums del cliente es: ', contImg
    end subroutine

end module listaAlbumss