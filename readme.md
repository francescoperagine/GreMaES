# GreMaES

GreMaES is an expert system, written in Prolog, for managing plants in a greenhouse. This project was developed as part of the Fundamentals of Artificial Intelligence course for the Master's degree program in Computer Science at the Department of Computer Science, University of Bari.

## Features

- **User mode**: The system interactively prompts the user to input symptoms observed on a plant. It then displays diagnoses and potential treatments, and offers the user the option to view the reasoning behind its conclusions.
- **Monitoring mode**: The system automatically acquires environmental data from sensors (temperature, humidity, image-to-caption) at regular intervals, communicates diagnoses, and activates/deactivates actuators as needed.
- **Knowledge base mode**: The system interactively prompts the user to select which information from the knowledge base they would like to consult.
- **Forward & Backward reasoning**: The system employs a hybrid approach to inference.
- **History management**: The system maintains a record of the facts and rules used to reach diagnoses.

## Installation

GreMaES requires the [YAP Prolog 6.2.2](http://lacam.di.uniba.it/~ferilli/ufficiale/corsi/ai-eng/yap6.2.2.zip) interpreter to run.

## Usage

To start the software, follow these steps:

```
git clone https://github.com/francescoperagine/GreMaES.git
cd GreMaES
yap
[main].
start.
```



## Contributing

If you would like to contribute to this project, please feel free to submit a pull request or open an issue.

## License

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT) - see the [LICENSE](LICENSE.txt) file for details.
