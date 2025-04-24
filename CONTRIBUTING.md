# Contributing to the US Social Determinants of Health Dataset

Thank you for your interest in contributing to the US Social Determinants of Health Dataset! This guide outlines the process for contributing to this project.

## Code of Conduct

This project follows a code of conduct that all contributors are expected to adhere to. Please be respectful and considerate of others when participating.

## How to Contribute

There are several ways to contribute to this project:

1. **Bug fixes**: Help fix issues in the existing codebase
2. **New features**: Add new functionality to enhance the pipeline
3. **Documentation**: Improve or expand the documentation
4. **Data sources**: Add support for additional data sources
5. **Testing**: Add or improve tests
6. **Performance optimizations**: Enhance speed or memory usage
7. **Visualization**: Improve map and chart generation

## Getting Started

1. **Fork the repository**: Create your own fork of the project
2. **Clone your fork**: `git clone https://github.com/yourusername/US-SocialDeterminantsOfHealth.git`
3. **Create a branch**: `git checkout -b feature/your-feature-name`
4. **Install dependencies**: `Rscript R/install_packages.r`
5. **Make your changes**: Follow the coding style and guidelines
6. **Test your changes**: Run appropriate tests to verify your code
7. **Commit your changes**: Use clear, descriptive commit messages
8. **Push to your fork**: `git push origin feature/your-feature-name`
9. **Create a Pull Request**: Submit a PR from your fork to the main repository

## Development Environment

Before starting development, ensure you have:

1. R version 4.0.0 or newer
2. Appropriate system dependencies for spatial packages
3. Optional: RStudio for easier R development
4. Git and GitHub account

## Coding Style

Please follow these guidelines for your code contributions:

- Use snake_case for variables and functions
- Four-space indentation
- Maximum 80 characters per line (soft limit, can exceed when necessary)
- Include documentation for all functions and complex code sections
- Add appropriate error handling
- Follow the modular architecture pattern for new modules

## Documentation

When documenting your code:

- Include a description of what each function does
- Document parameters and return values
- Add examples where appropriate
- Update any relevant README files or guides
- For new variables, update the data dictionary

## Testing

All new code should include appropriate tests:

- Unit tests for individual functions
- Integration tests for modules
- End-to-end tests for complete pipelines
- Performance tests for optimization-focused changes

Use the existing test framework and patterns in the codebase.

## Pull Request Process

1. Ensure your code meets all guidelines
2. Update documentation as needed
3. Make sure all tests pass
4. Submit a PR with a clear description of your changes
5. Respond to any feedback from reviewers
6. Wait for approval and merge

## Adding New Data Sources

When adding support for a new data source:

1. Create a new `fetch_[source]_data.r` file
2. Add appropriate attribution and citations
3. Include fallback mechanisms for offline mode
4. Add the source to the variable crosswalk
5. Document the new variables in the data dictionary
6. Add appropriate tests
7. Update the README to include the new source

## Reporting Issues

If you find bugs or have feature requests, please create an issue in the GitHub repository with:

1. A clear, descriptive title
2. A detailed description of the issue or request
3. Steps to reproduce (for bugs)
4. Expected vs. actual behavior
5. Screenshots if applicable
6. System information (OS, R version, etc.)

## Contact

For any questions about contributing, please contact the project maintainer:
David Lary (davidlary@me.com)