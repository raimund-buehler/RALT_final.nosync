import matplotlib.pyplot as plt
import pandas as pd

def hist(path):
    # Load the results data from the uploaded CSV file
    results_path = path
    results_data = pd.read_csv(results_path)

    # Display the first few rows to verify the data
    results_data.head()

    # Set up the figure and axes for the histograms
    fig, axes = plt.subplots(nrows=1, ncols=6, figsize=(18, 6))

    # Titles for each subplot
    titles = ["Alpha_Win", "Theta_Win", "Rho_Win", "Alpha_Loss", "Theta_Loss", "Rho_Loss"]

    # Plot histograms
    for ax, column, title in zip(axes, titles, titles):
        ax.hist(results_data[column], bins=20, color='skyblue', edgecolor='black')
        ax.set_title(f'Histogram of {title}')
        ax.set_xlabel(title)
        ax.set_ylabel('Frequency')

    # Show the plots
    plt.tight_layout()
    plt.show()
