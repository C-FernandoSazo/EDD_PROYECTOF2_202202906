module lista_clientes
    implicit none

    type, public :: Cliente 
        character(len=20) :: nombre, password, dpi
    end type Cliente

    type, public :: NodoCliente
        type(Cliente) :: cliente
        type(NodoCliente), pointer :: siguiente => null()
    end type NodoCliente

    type, public :: listaClientes
        type(NodoCliente), pointer :: head => null()
        contains
            procedure :: agregarCliente
            procedure :: printlista
            procedure :: buscarCliente
    end type listaClientes

contains 

    subroutine agregarCliente(lista,client)
        class(listaClientes), intent(inout) :: lista
        type(Cliente), intent(inout) :: client
        type(NodoCliente), pointer :: nuevoNodo, actual

        allocate(nuevoNodo)
        nuevoNodo%cliente = client
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

        write(*,'(A, A)') 'SE AGREGO EL CLIENTE ', trim(client%nombre)
    end subroutine agregarCliente

    subroutine printlista(lista)
        class(listaClientes), intent(in) :: lista
        type(NodoCliente), pointer :: actual
        actual => lista%head
        do while(associated(actual))
            write(*,'(A,A,A,A,A,A)') "Cliente ", trim(actual%cliente%nombre), " dpi: ", trim(actual%cliente%dpi), &
            " pass: ", trim(actual%cliente%password)
            actual => actual%siguiente
        end do
    end subroutine printlista

    function buscarCliente(lista,nombre,pass) result(clienteTemp)
        class(listaClientes), intent(in) :: lista
        character(len=20), intent(in) :: nombre,pass
        type(NodoCliente), pointer :: actual
        type(Cliente) :: clienteTemp

        clienteTemp%dpi = "0" 
        clienteTemp%nombre = "0"
        clienteTemp%password = "0"

        actual => lista%head
        do while(associated(actual))
            if (actual%cliente%nombre == nombre .and. actual%cliente%password == pass) then
                clienteTemp = actual%cliente
            end if
            actual => actual%siguiente
        end do
    end function buscarCliente

end module lista_clientes