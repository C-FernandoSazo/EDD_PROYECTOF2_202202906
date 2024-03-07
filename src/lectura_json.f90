module lecturaJson
    use json_module
    use lista_clientes
    implicit none
contains

    subroutine leerClientes(lista, filename)
        type(listaClientes), intent(inout) :: lista
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
            
            call lista%agregarCliente(clienteTemp)
            
        end do
    
        ! Finalizar la biblioteca JSON
        call json%destroy()
    
    end subroutine leerClientes

    subroutine leerCapas(arbol, filename)
        use Arbol_Capas
        use matriz_dispersa
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
                end if
                write(*,*) key
                ! Extraer el arreglo de pixeles
                call matrizTemp%vaciarMatrix()
                call jCore%get_child(pJsonValue, 'pixeles', pPixelsArray, found=found)
                if (found) then
                    call jCore%info(pPixelsArray, n_children=nPixeles)
                    ! Recorrer cada pixel en la capa
                    do j = 1, nPixeles
                        print *, "ENTRO A PIXELES"
                        call jCore%get_child(pPixelsArray, j, pPixelValue, found=found)
                        if (found) then
                            ! Extraer fila, columna y color de cada pixel
                            call jCore%get_child(pPixelValue, 'fila', attributePointer, found=found)
                            if (found) then 
                                call jCore%get(attributePointer, fila)
                                write(*,*) fila
                            end if

                            call jCore%get_child(pPixelValue, 'columna', attributePointer, found=found)
                            if (found) then 
                                call jCore%get(attributePointer, columna)
                                write(*,*) columna
                            end if

                            call jCore%get_child(pPixelValue, 'color', attributePointer, found=found)
                            if (found) then 
                                call jCore%get(attributePointer, colorTemp)
                                color = colorTemp
                                write(*,*) color
                            end if
                            call matrizTemp%add(fila,columna,color)
                        end if
                    end do
                end if
                call arbol%insertarNodo(key,matrizTemp)
            end if
        end do

        ! Finalizar la biblioteca JSON
        call json%destroy()
    end subroutine leerCapas

end module lecturaJson