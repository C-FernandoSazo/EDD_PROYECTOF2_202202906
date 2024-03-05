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



end module