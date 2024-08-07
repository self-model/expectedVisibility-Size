% Assuming best_parameters and parameters_to_estimate are your matrices
nParameters = size(best_parameters, 2); % Number of parameters (columns)

% Create a figure to hold all subplots
figure;

for i = 1:nParameters
    subplot(ceil(nParameters/2), 2, i); % Arrange in a grid
    scatter(best_parameters(:, i), parameters_to_estimate(:, i), 'w'); % Plot with white color just to get the scale
    hold on;
    
    % Plot identity line
    minX = min([best_parameters(:, i); parameters_to_estimate(:, i)]);
    maxX = max([best_parameters(:, i); parameters_to_estimate(:, i)]);
    plot([minX maxX], [minX maxX], 'k--'); % 'k--' creates a black dashed line
    
    % Annotate points with row numbers
    for j = 1:size(best_parameters, 1)
        text(best_parameters(j, i), parameters_to_estimate(j, i), num2str(j), 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle');
    end
    
    title(sprintf('Parameter %d', i));
    xlabel('True Value');
    ylabel('Estimated Value');
    hold off; % Release the subplot for the next iteration
end

% Adjust the layout
sgtitle('True vs. Estimated Parameter Values');
