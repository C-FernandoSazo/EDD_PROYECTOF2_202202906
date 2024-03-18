module Arbol_clientes
    implicit none

    integer, parameter :: MAXI = 4, MINI = 2 

    type:: Cliente
        integer :: id
        character(len=20) :: nombre, password, dpi
    end type Cliente

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr

    type BTreeNode
        type(Cliente) :: cliente(0:MAXI+1)
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)
    end type BTreeNode

    type ArbolClientes 
        integer :: contador = 1
        type(BTreeNode), pointer :: root => null()
        contains 
            procedure :: insert
            procedure :: setValue
            procedure :: insertNode
            procedure :: splitNode
            procedure :: createNode
            procedure :: traversal
            procedure :: buscar
            procedure :: imprimirClientes
            procedure :: modificarCliente
    end type ArbolClientes

contains

    subroutine insert(this, val)
        class(ArbolClientes), intent(inout) :: this
        type(Cliente), intent(inout) :: val
        type(Cliente):: i
        type(BTreeNode), pointer :: child
        allocate(child)
        val%id = this%contador
        this%contador =  this%contador + 1
        write (*,*) "ESTAMOS CON: ",val%id, val%nombre
        if (this%setValue(val, i, this%root, child)) then
                this%root => this%createNode(i, child)
        end if
    end subroutine insert

    recursive function setValue(this,val, pval, node, child) result(res)
        class(ArbolClientes), intent(inout) :: this
        type(Cliente), intent(in) :: val
        type(Cliente), intent(inout) :: pval
        type(BTreeNode), pointer, intent(inout) :: node
        type(BTreeNode), pointer, intent(inout) :: child
        type(BTreeNode), pointer :: newnode        
        integer :: pos
        logical :: res
        allocate(newnode)
        if (.not. associated(node)) then            
                pval = val
                child => null()
                res = .true.
                return
        end if
        if (val%id < node%cliente(1)%id) then
                pos = 0
        else
                pos = node%num
                do while (val%id < node%cliente(pos)%id .and. pos > 1) 
                pos = pos - 1
                end do
                if (val%id == node%cliente(pos)%id .or. val%nombre == node%cliente(pos)%nombre) then
                    print *, "Elemento duplicado!"
                    res = .false.
                    return
                end if
        end if
        if (this%setValue(val, pval, node%link(pos)%ptr, child)) then
                if (node%num < MAXI) then
                    call this%insertNode(pval, pos, node, child)
                else
                    call this%splitNode(pval, pval, pos, node, child, newnode)
                    child => newnode
                    res = .true.
                return
            end if
        end if
        res = .false.
    end function setValue

    subroutine insertNode(this,val, pos, node, child)
        class(ArbolClientes), intent(inout) :: this
        type(Cliente), intent(in) :: val
        integer, intent(in) :: pos
        type(BTreeNode), pointer, intent(inout) :: node
        type(BTreeNode), pointer, intent(in) :: child
        integer :: j
        j = node%num
        do while (j > pos)
                node%cliente(j + 1) = node%cliente(j)
                node%link(j + 1)%ptr => node%link(j)%ptr
                j = j - 1
        end do
        node%cliente(j + 1) = val
        node%link(j + 1)%ptr => child
        node%num = node%num + 1
    end subroutine insertNode

    subroutine splitNode(this,val, pval, pos, node, child, newnode)
        class(ArbolClientes), intent(inout) :: this
        type(Cliente), intent(in) :: val
        integer, intent(in) :: pos
        type(Cliente), intent(inout) :: pval
        type(BTreeNode), pointer, intent(inout) :: node,  newnode
        type(BTreeNode), pointer, intent(in) ::  child
        integer :: median, i, j
        if (pos > MINI) then
                median = MINI + 1
        else
                median = MINI
        end if
        if (.not. associated(newnode)) then
            allocate(newnode)
        do i = 0, MAXI
                    newnode%link(i)%ptr => null()
            enddo
        end if
        j = median + 1
        do while (j <= MAXI)
                newnode%cliente(j - median) = node%cliente(j)
                newnode%link(j - median)%ptr => node%link(j)%ptr
                j = j + 1
        end do
        node%num = median
        newnode%num = MAXI - median
        if (pos <= MINI) then
                call this%insertNode(val, pos, node, child)
        else
                call this%insertNode(val, pos - median, newnode, child)
        end if        
        pval = node%cliente(node%num)        
        newnode%link(0)%ptr => node%link(node%num)%ptr
        node%num = node%num - 1
    end subroutine splitNode

    function createNode(this,val, child) result(newNode)
        class(ArbolClientes), intent(inout) :: this
        type(Cliente), intent(in) :: val
        type(BTreeNode), pointer, intent(in) :: child
        type(BTreeNode), pointer :: newNode
        integer :: i
        allocate(newNode)
        newNode%cliente(1) = val
        newNode%num = 1
        newNode%link(0)%ptr => this%root
        newNode%link(1)%ptr => child
        do i = 2, MAXI
                newNode%link(i)%ptr => null()
        end do
    end function createNode

    recursive subroutine traversal(this,myNode)
        class(ArbolClientes), intent(inout) :: this
        type(BTreeNode), pointer, intent(in) :: myNode
        integer :: i
        if (associated(myNode)) then
                write (*, '(A)', advance='no') ' [ '
                i = 0
                do while (i < myNode%num)
                    write (*,'(1I3)', advance='no') myNode%cliente(i+1)%id
                    i = i + 1
                end do
                do i = 0, myNode%num
                    call this%traversal(myNode%link(i)%ptr)    
                end do
                write (*, '(A)', advance='no') ' ] '
        end if
    end subroutine traversal

    function buscar(this, nombre, password) result(clienteEncontrado)
        class(ArbolClientes), intent(in) :: this
        character(len=20), intent(in) :: nombre, password
        type(Cliente), pointer :: clienteEncontrado 
        type(BTreeNode), pointer :: actual
        clienteEncontrado%nombre = "0"
        actual => this%root
        call buscarRecursivoPorNombreYPassword(actual, nombre, password, clienteEncontrado)
    end function buscar
    
    recursive subroutine buscarRecursivoPorNombreYPassword(nodo, nombre, password, clienteEncontrado)
        type(BTreeNode), pointer, intent(in) :: nodo
        character(len=*), intent(in) :: nombre, password
        type(Cliente), pointer, intent(inout) :: clienteEncontrado
        integer :: i
    
        if (.not. associated(nodo)) then
            return
        end if
    
        do i = 1, nodo%num
            write (*,*) "ESTAMOS CON: ", trim(nodo%cliente(i)%nombre), " ", trim(nodo%cliente(i)%password)
            if (trim(nombre) == trim(nodo%cliente(i)%nombre) .and. trim(password) == trim(nodo%cliente(i)%password)) then
                clienteEncontrado => nodo%cliente(i)
                return
            end if
        end do
    
        do i = 0, nodo%num
            call buscarRecursivoPorNombreYPassword(nodo%link(i)%ptr, nombre, password, clienteEncontrado)
        end do
    end subroutine buscarRecursivoPorNombreYPassword

    subroutine imprimirClientes(this)
        class(ArbolClientes), intent(in) :: this
        type(BTreeNode), pointer :: actual
        actual => this%root
        call imprimirRecursivo(actual)
    end subroutine imprimirClientes

    recursive subroutine imprimirRecursivo(nodo)
        type(BTreeNode), pointer, intent(in) :: nodo
        integer :: i

        if (.not. associated(nodo)) then
            return
        end if

        do i = 1, nodo%num
            write (*,'(I0,A,A,A,A)') nodo%cliente(i)%id,". Nombre: ", trim(nodo%cliente(i)%nombre), &
            ", DPI: ", trim(nodo%cliente(i)%dpi)
        end do
    
        do i = 0, nodo%num
            call imprimirRecursivo(nodo%link(i)%ptr)
        end do
    end subroutine
    
    subroutine modificarCliente(this, id, nombre, password, dpi) 
        class(ArbolClientes), intent(in) :: this
        character(len=20), intent(in) :: nombre, password, dpi
        integer, intent(in) :: id
        type(BTreeNode), pointer :: actual
        actual => this%root
        call modificarRecursivo(actual,id,nombre,password, dpi)
    end subroutine modificarCliente
    
    recursive subroutine modificarRecursivo(nodo, id, nombre, password, dpi)
        type(BTreeNode), pointer, intent(in) :: nodo
        character(len=20), intent(in) :: nombre, password, dpi
        integer, intent(in) :: id
        integer :: i
    
        if (.not. associated(nodo)) then
            return
        end if
    
        do i = 1, nodo%num
            write (*,*) "ESTAMOS CON: ", trim(nodo%cliente(i)%nombre), " ", trim(nodo%cliente(i)%password)
            if (nodo%cliente(i)%id == id) then
                    nodo%cliente(i)%nombre = nombre
                    nodo%cliente(i)%password = password
                    nodo%cliente(i)%dpi = dpi
                    print *,"CAMBIO REALIZADO"
                return
            end if
        end do
    
        do i = 0, nodo%num
            call modificarRecursivo(nodo%link(i)%ptr,id,nombre,password,dpi)
        end do
    end subroutine modificarRecursivo
    
    
end module Arbol_clientes