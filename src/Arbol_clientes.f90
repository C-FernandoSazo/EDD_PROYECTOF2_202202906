module arbol_clientes
    implicit none

    type, public :: NodoClient
        integer :: keys(5)
        integer :: num_keys = 0
        type(NodoClient), pointer :: hijo(:)
        logical :: hoja = .true.
    end type NodoClient

    type arbolClientes
        type(NodoClient), pointer :: raiz 
    end type arbolClientes

contains
    subroutine insert(tree, key)
        type(arbolClientes), intent(inout) :: tree
        integer, intent(in) :: key
        
    end subroutine insert

end module arbol_clientes