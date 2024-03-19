module lecturaJson
    use json_module
    use Arbol_clientes
    use matriz_DispersaI
    use Arbol_CapaBss
    implicit none
contains

    subroutine leerClientes(arbol, filename)
        type(ArbolClientes), intent(inout) :: arbol
        character(len=*), intent(in) :: filename
        type(json_file) :: json
        type(json_core) :: jCore
        type(json_value), pointer :: pJsonArray, pJsonValue, attributePointer
        type(Cliente) :: clienteTemp
        integer :: i, nItems
        logical :: found
        character(:), allocatable :: nombre, dpi, password
    
        ! Inicializar la biblioteca JSON
        call json%initialize()
        call json%load_file(filename)
        call json%info('', n_children=nItems)
        ! Obtener el objeto json_core asociado con el archivo json actualmente abierto
        call json%get_core(jCore)
    
        ! Obtener el puntero al array JSON
        call json%get('', pJsonArray, found)
        if (.not. found) then
            print *, "Error: El archivo JSON no contiene un objeto."
            call json%destroy()
            return
        end if
    
        do i = 1, nItems
            call jCore%get_child(pJsonArray, i, pJsonValue, found=found)

            call jCore%get_child(pJsonValue, 'dpi', attributePointer, found=found)
            if (found) then 
                call jCore%get(attributePointer, dpi)
                clienteTemp%dpi = dpi
            end if

            call jCore%get_child(pJsonValue, 'nombre_cliente', attributePointer, found=found)
            if (found) then 
                call jCore%get(attributePointer, nombre)
                clienteTemp%nombre = nombre
            end if

            call jCore%get_child(pJsonValue, 'password', attributePointer, found=found)
            if (found) then 
                call jCore%get(attributePointer, password)
                clienteTemp%password = password
            end if
            call arbol%insert(clienteTemp)
            
        end do
        call arbol%traversal(arbol%root)
        ! Finalizar la biblioteca JSON
        call json%destroy()
    
    end subroutine leerClientes

    subroutine leerCapas(arbol, filename)
        type(ArbolCapas), intent(inout) :: arbol
        character(len=*), intent(in) :: filename
        type(json_file) :: json
        type(json_core) :: jCore
        type(json_value), pointer :: pJsonArray, pJsonValue, pPixelsArray, pPixelValue, attributePointer
        type(matrizDispersa) :: matrizTemp
        integer :: i, j, nCapas, nPixeles, fila, columna, key
        character(len=:), allocatable :: colorTemp
        character(len=7) :: color
        logical :: found

        ! Inicializar la biblioteca JSON
        call json%initialize()
        call json%load_file(filename)
        call json%info('', n_children=nCapas)

        ! Obtener el objeto json_core asociado con el archivo json actualmente abierto
        call json%get_core(jCore)

        ! Obtener el puntero al array JSON principal (capas)
        call json%get('', pJsonArray, found)
        if (.not. found) then
            print *, "Error: El archivo JSON no contiene un objeto."
            call json%destroy()
            return
        end if

        ! Recorrer cada capa
        do i = 1, nCapas
            call jCore%get_child(pJsonArray, i, pJsonValue, found=found)
            if (found) then
                ! Extraer el id_capa
                call jCore%get_child(pJsonValue, 'id_capa', attributePointer, found=found)
                if (found) then
                    call jCore%get(attributePointer, key)
                    call arbol%insertarNodo(key)
                end if
                write(*,*) key
                ! Extraer el arreglo de pixeles
                call jCore%get_child(pJsonValue, 'pixeles', pPixelsArray, found=found)
                if (found) then
                    call jCore%info(pPixelsArray, n_children=nPixeles)
                    do j = 1, nPixeles
                        call jCore%get_child(pPixelsArray, j, pPixelValue, found=found)
                        if (found) then
                            ! Extraer fila, columna y color de cada pixel
                            call jCore%get_child(pPixelValue, 'fila', attributePointer, found=found)
                            if (found) then 
                                call jCore%get(attributePointer, fila)
                            end if

                            call jCore%get_child(pPixelValue, 'columna', attributePointer, found=found)
                            if (found) then 
                                call jCore%get(attributePointer, columna)
                            end if

                            call jCore%get_child(pPixelValue, 'color', attributePointer, found=found)
                            if (found) then 
                                call jCore%get(attributePointer, colorTemp)
                                color = colorTemp
                            end if
                            call arbol%ingresarMatriz(key,fila,columna,color)
                        end if
                    end do
                end if
            end if
        end do

        ! Finalizar la biblioteca JSON
        call json%destroy()
    end subroutine leerCapas

    subroutine leerImagenes(arbol, miArbolCapas, filename)
        use Arbol_Imagenes
        type(ArbolImagenes), intent(inout) :: arbol
        character(len=*), intent(in) :: filename
        type(ArbolCapas), intent(inout) :: miArbolCapas
        type(Capa) :: capaTemp
        type(json_file) :: json
        type(json_core) :: jCore
        type(json_value), pointer :: pJsonArray, pJsonValue, pCapasArray, CapaValue, attributePointer
        integer :: i, j, nCapas, nImagenes, id, valor
        logical :: found

        ! Inicializar la biblioteca JSON
        call json%initialize()
        call json%load_file(filename)
        call json%info('', n_children=nImagenes)

        ! Obtener el objeto json_core asociado con el archivo json actualmente abierto
        call json%get_core(jCore)

        ! Obtener el puntero al array JSON principal (capas)
        call json%get('', pJsonArray, found)
        if (.not. found) then
            print *, "Error: El archivo JSON no contiene un objeto."
            call json%destroy()
            return
        end if

        do i = 1, nImagenes
            call jCore%get_child(pJsonArray, i, pJsonValue, found=found)
            if (found) then
                call jCore%get_child(pJsonValue, 'id', attributePointer, found=found)
                if (found) then
                    call jCore%get(attributePointer, id)
                    write(*,'(A,I0)') "ESTE ES EL ID: ",id
                    call arbol%insertar(id)
                end if
                print *, "EXTRAYENDO CAPAS"
                call jCore%get_child(pJsonValue, 'capas', pCapasArray, found=found)
                if (found) then
                    call jCore%info(pCapasArray, n_children=nCapas)
                    do j = 1, nCapas
                        call jCore%get_child(pCapasArray, j,  CapaValue, found=found)
                        if (found) then
                            call jCore%get(CapaValue, valor)
                            write(*,'(A,I0)') "CAPA: ", valor
                            print *,"ENCONTRADO LA MATRIZ"
                            capaTemp = miArbolCapas%buscarNodo(valor)
                            print *,"MATRIZ ENCONTRADA"
                            call arbol%ingresarCapas(id,capaTemp%key,capaTemp%matriz)         
                        end if
                    end do 
                end if
            end if
        end do

        ! Finalizar la biblioteca JSON
        call json%destroy()
    end subroutine leerImagenes

    subroutine leerAlbumes(miListaAlbum, filename)
        use listaAlbum
        type(lista_album), intent(inout) :: miListaAlbum
        character(len=*), intent(in) :: filename
        type(json_file) :: json
        type(json_core) :: jCore
        type(json_value), pointer :: pJsonArray, pJsonValue, pImgsArray, ImgValue, attributePointer
        integer :: i, j, nImgs, nAlbumes, valor
        character(len=:), allocatable :: albumTemp
        character(len=10) :: album
        logical :: found

        ! Inicializar la biblioteca JSON
        call json%initialize()
        call json%load_file(filename)
        call json%info('', n_children=nAlbumes)

        ! Obtener el objeto json_core asociado con el archivo json actualmente abierto
        call json%get_core(jCore)

        ! Obtener el puntero al array JSON principal (capas)
        call json%get('', pJsonArray, found)
        if (.not. found) then
            print *, "Error: El archivo JSON no contiene un objeto."
            call json%destroy()
            return
        end if

        do i = 1, nAlbumes
            call jCore%get_child(pJsonArray, i, pJsonValue, found=found)
            if (found) then
                call jCore%get_child(pJsonValue, 'nombre_album', attributePointer, found=found)
                if (found) then
                    call jCore%get(attributePointer, albumTemp)
                    album = albumTemp
                    call miListaAlbum%agregarAlbum(album)
                end if
                print *, "EXTRAYENDO IMAGENES"
                call jCore%get_child(pJsonValue, 'imgs', pImgsArray, found=found)
                if (found) then
                    call jCore%info(pImgsArray, n_children=nImgs)
                    do j = 1, nImgs
                        call jCore%get_child(pImgsArray, j,  ImgValue, found=found)
                        if (found) then
                            call jCore%get(ImgValue, valor)
                            call miListaAlbum%addImagen(album,valor)
                        end if
                    end do 
                end if
            end if
        end do

        ! Finalizar la biblioteca JSON
        call json%destroy()
    end subroutine leerAlbumes

end module lecturaJson