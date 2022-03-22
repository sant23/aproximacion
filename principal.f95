program minimos
    use lineal
    use aproximacion 

    implicit none
    real(8), allocatable :: Xn(:), Yn(:), A(:,:), B(:), X(:)
    character(15)::fichero 
    integer :: i,l,j,k,n,m 
    
    n=3
    
    print*, 'Introduzca el valor de m'
    read*, m 
    allocate (Xn(n),Yn(n), A(m+1,m+1), B(m+1), X(m+1))

    
    fichero = 'datos.dat'
    open(unit=10, file = fichero)
   
    
    do i=1, n   
        read(10,*) Xn(i), Yn(i)
    ENDDO 

    call minimos_cuadrados(a,b,x,xn,yn)

    write(*,*) '-------b--------'
    write(*,*)   b  
    !write(*,*) '-------a--------'
    !write(*,fmt='(6(f7.2,1x))') a 


    write (*,*) '------vector xn-------'
    write(*,*) xn
    write (*,*) '------vector yn-------'
    write(*,*) yn
    


end program  minimos