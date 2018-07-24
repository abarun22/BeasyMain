module InterfaceSolutionCalculator_Module
    use, intrinsic :: iso_fortran_env;
    use DataStructure_Module;
    use Utility_Module;                                        !==> TO DELETE?
    implicit none;
    private;
    public :: DDDMS_InterfaceSolutionCalculator;
    public :: DDDMS_SMInterfaceSolutionCalculator;

    !╔════════════════════════════════════════════╗
    !║DDDMS_INTERFACESOLUTIONCALCULATOR DEFINITION║
    !╚════════════════════════════════════════════╝
    !┌────────────────────────────────────────────────────────────────────────────┐
    !│ABSTRACT DDDMS_INTERFACESOLUTIONCALCULATOR USER-DEFINED DATA TYPE DEFINITION│
    !└────────────────────────────────────────────────────────────────────────────┘
    type, abstract :: DDDMS_InterfaceSolutionCalculator
        
    contains 
        procedure(CalculateABTRACT), deferred :: calculate;
        
    end type DDDMS_InterfaceSolutionCalculator
    
    !┌─────────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_LOCALSCHURCOMPLEMENTCALCULATOR ABSTRACT USER-DEFINED DATA TYPE PROCEDURE DEFINITION│
    !└─────────────────────────────────────────────────────────────────────────────────────────┘
    abstract interface
        subroutine CalculateABTRACT(self, inputData)
            import;
            class(DDDMS_InterfaceSolutionCalculator) :: self;
            class(DDDMS_DataContainer)               :: inputData;
        end subroutine CalculateABTRACT
    end interface

    !┌────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    !│CONCRETE DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE DEFINITION FOR SHARED MEMORY ARCHITECTURE│
    !└────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    type, extends(DDDMS_InterfaceSolutionCalculator) :: DDDMS_SMInterfaceSolutionCalculator
        
    contains 
        procedure :: calculate => calculateOnSM;
        
    end type DDDMS_SMInterfaceSolutionCalculator

    !┌────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└────────────────────────────────────────────────────────────────────────────────────┘
    interface DDDMS_SMInterfaceSolutionCalculator
        module procedure NewDDDMS_SMInterfaceSolutionCalculator; ! ADD CONSTRUCTOR TO DDDMS_SMInterfaceSolutionCalculator GENERIC INTERFACE
    end interface DDDMS_SMInterfaceSolutionCalculator

contains

    !┌────────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└────────────────────────────────────────────────────────────────────────────────────────┘
    type(DDDMS_SMInterfaceSolutionCalculator) function NewDDDMS_SMInterfaceSolutionCalculator(self)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMInterfaceSolutionCalculator) :: self;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        
    end function NewDDDMS_SMInterfaceSolutionCalculator
    
    !┌────────────────────────────────────┐
    !│"CALCULATE" PROCEDURE IMPLEMENTATION│
    !└────────────────────────────────────┘
    subroutine calculateOnSM(self, inputData)
    
        implicit none;
    
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMInterfaceSolutionCalculator) :: self;
        class(DDDMS_DataContainer)                 :: inputData;
        integer(INT64), allocatable                :: pivot(:);
        integer(INT32)                             :: info;
        integer(INT32)                             :: status;
         
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘

        !┌─────────────────────────────────────────────────────────────────┐
        !│ALLOCATE THE INTEGER VECTOR "PIVOT" CONTAINING THE PIVOT INDICES │
        !└─────────────────────────────────────────────────────────────────┘
        allocate( pivot( size( inputData%SGlob, 1 ) ), STAT = status );
        if(status /= 0) then
            print*, "Allocation of array pivot failed!.";
            print*, "Error code: ", status;
            pause;
            stop;
        else
            pivot = 0.0;
        end if

        !┌───────────────────────────────────────────────────────────────────────────────────────────┐
        !│FIND THE SOLUTION OF THE EQUATIONS (SGLOBAL)A*X = (GGLOBAL)B USING THE LAPACK ROUTINE SGESV│
        !└───────────────────────────────────────────────────────────────────────────────────────────┘
       	call DGESV(size( inputData%SGlob, 1 ), & !IS THE ORDER N OF MATRIX A AND THE NUMBER OF ROWS OF MATRIX B.
                   1,                          & !IS THE NUMBER OF RIGHT-HAND SIDES; THAT IS, THE NUMBER OF COLUMNS OF MATRIX B.
                   inputData%SGlob,            & !S THE GENERAL MATRIX A TO BE FACTORED.
                   size( inputData%SGlob, 1 ), & !IS THE LEADING DIMENSION OF THE ARRAY SPECIFIED FOR A.
                   pivot,                      & 
                   inputData%gGlob,            & !IS THE GENERAL MATRIX B, CONTAINING THE NRHS RIGHT-HAND SIDES OF THE SYSTEM. 
                   size( inputData%gGlob, 1 ), & !IS THE LEADING DIMENSION OF THE ARRAY SPECIFIED FOR B.
                   info);

        if( info > 0 ) then
            write(*,*)'The diagonal element of the triangular factor of inputData%SGlob,';
            write(*,*)'U(',info,',',info,') is zero, so that';
            write(*,*)'inputData%SGlob is singular; the solution could not be computed.';
            pause;
            stop;
        end if
        
        !┌────────────────────────────────────────────────────────────┐
        !│ALLOCATE AND INITIALISE VECTOR INPUTDATA%U_INTERFACESOLUTION│
        !└────────────────────────────────────────────────────────────┘
        allocate( inputData%U_InterfaceSolution( 3*size( inputData%IndependenInterfaceNodesArray, 1 ) ) , STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
        if(status /= 0) then
            print*, "Failed allocation of U_InterfaceSolution!";
            print*, "Errore code: ", status;
            pause;
            stop;
        else
            inputData%U_InterfaceSolution = 0.0; 
        end if        
        
        allocate( inputData%f_InterfaceSolution( 3*size( inputData%IndependenInterfaceNodesArray, 1 ) ) , STAT = status );
        if(status /= 0) then
            print*, "Failed allocation of f_InterfaceSolution!";
            print*, "Errore code: ", status;
            pause;
            stop;
        else
            inputData%f_InterfaceSolution = 0.0; 
        end if        
        
        !┌─────────────────────────────────────────────────────────────────┐
        !│FILL THE VECTOR GGLOB USING THE SOLUTION OF THE EQUATIONS A*X = B│
        !└─────────────────────────────────────────────────────────────────┘
        inputData%U_InterfaceSolution = inputData%gGlob;
        
        write(10,*)'Size of IndependenInterfaceNodesArray:'
        write(10,*)size(inputData%IndependenInterfaceNodesArray, 1)
        
        write(10,*)'Size of interface solution array:'
        write(10,*)size(inputData%U_InterfaceSolution)
        
        !┌────────────────────────────────────────┐
        !│DEALLOCATE TEMPORARY SUPPORT ARRAY PIVOT│
        !└────────────────────────────────────────┘
        if( allocated( pivot ) ) then
            deallocate( pivot, STAT = status );
            if(status /= 0) then
                print*, "Deallocation of array pivot failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            end if
        end if
        
        !────────────────────────────────────────────────────           
        !====================TO DEBUD========================
!DEC$ IF DEFINED(_DEBUG)
        call Utility_OutputDataVector( "Output_FORTRAN_Interface_Solution_Vector.out",  inputData%U_InterfaceSolution );
!DEC$ ENDIF
        !====================TO DEBUD========================
        !─────────────────────────────────────────────────────
       
    end subroutine calculateOnSM    
    
end module InterfaceSolutionCalculator_Module