import chalk from 'chalk';
import chalkAnimation from 'chalk-animation';
import inquirer from 'inquirer';
import { createSpinner } from 'nanospinner';
import figlet from 'figlet';
import gradient from 'gradient-string';

const sleep = (ms = 2000) => new Promise((r) => setTimeout(r, ms));

let name: string

async function welcome() {
    const rainbowTitle = chalkAnimation.rainbow(
        'Hello from Chalk! :) \n'
    );

    await sleep();
    rainbowTitle.stop();

    console.log(`
      ${chalk.bgBlue('I am blue \n')} 
      ${chalk.bgRed('I am red')}
    `);
}

async function handleAnswer(isCorrect: boolean) {
    const spinner = createSpinner('Checking answer...').start();
    await sleep();

    if (isCorrect) {
        spinner.success({ text: `Nice work!` });
    } else {
        spinner.error({ text: `Game over, so sorry!` });
        process.exit(1);
    }
}

async function askName() {
    const answers = await inquirer.prompt({
        name: 'name',
        type: 'input',
        message: 'What is your name?',
        default() {
            return 'Name';
        },
    });

    name = answers.name;
}

function final() {
    // console.clear();
    figlet(`Congrats , ${name} !`, (err, data) => {
        console.log(gradient.pastel.multiline(data) + '\n');

        console.log(
            chalk.green(
                `Manny Pink Circles will be rewarded! ;)`
            )
        );
        process.exit(0);
    });
}

async function question1() {
    const answers = await inquirer.prompt({
        name: 'question_1',
        type: 'list',
        message: 'WHich way do we go? \n',
        choices: [
            'Hello driven development',
            'Finite state machine',
            'Rubber duck'
        ],
    });

    return handleAnswer(answers.question_1 === 'Hello driven development');
}

console.clear();
await welcome();
await askName();
await question1();
final()