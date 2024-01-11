%% Instructions:
% This program can be ran in a Linux environment by the following:
%
% 1. Open the matlab CLI with the following command:
%       matlab -nodisplay -nosplash -nodesktop;
%
% 2. Execute the script by typing: 
%       [a, b, c] = munge(<SOME_INTEGER>);
%
% 3. Confirm the function outputs by typing `a`, `b`, or `c` into the CLI
%    and pressing enter to see the respective outputs for A, B, and R

%%  Description:
% This program takes an input integer `n` and creates an `n` matrix if `n`
% is greater than 9. It then slices the matrix to 6x6 and multiplies it by
% its transpose to get matrix A.
%
% A is then decomposed into L and U by Cholesky decomposition. L and U are
% multiplied to get matrix B, and then matrix R is computed by R = B - A.
%

%% Begin Function Definition
function [R, A, B] = munge(n)
    % Upkeep variables
    flag = -1;
    NUM_ITERS = 10;
    sentinel = NUM_ITERS;
    
    % This condition will loop until we get a satisfactory symmetric positive
    % definite matrix or n is less than 9.
    while flag ~= 0 && sentinel > 0
        % Check if n < 9
        if n < 9
            R = 0;
            A = 0;
            B = 0;
            break;
        else
            % Generate random matrix M
            M = randi([-10,10], n);
            fprintf("Generating a %dx%d matrix:\n", n, n);
            disp("M = ");
            disp(M)
    
            % Extract slices of M
            M = M(4:9,2:7);
            fprintf("Slicing matrix:\n");
            disp("M =");
            disp(M)
    
            % Transpose matrix M
            Mt = M';
            disp("M' = ");
            disp(Mt);
    
            % Multiply M by M'
            A = M * Mt;
            disp("M * M' = ");
            disp(A);
        end
        
        % Perform Cholesky Decomposition
        [U, flag] = chol(A, "upper");
        [L, flag] = chol(A, "lower");
    
        if flag == 0
            fprintf("Performing Cholesky Decomposition:\n");
            fprintf("U = \n");
            disp(U);
            fprintf("L = \n");
            disp(L);
        end
    
        % Generate Matrix B
        B = U * L;
        fprintf("Multplying L * U: \nB = \n");
        disp(B);
    
        fprintf("Taking B - A:\nR = \n");
        R = B - A;
        disp(R);
        disp('__________________________________________________________________________________________________________________')
        A
        B
        R
        disp('__________________________________________________________________________________________________________________')
        
        % While loop upkeep
        sentinel = sentinel - 1;
        if sentinel == 0
            fprintf("\n\n\t\tNo matrix was found in %d iterations.\n\n", NUM_ITERS);
        end
    end
end

