type Particle = 'X' | '+' | '-' | 'P';

// problem - 3a
// method-1
function sortParticles(particles: Particle[]): Particle[] {

    particles.sort((a, b) => {
        switch (a) {
            case 'P':
                return 1;
            case '+':
                return b === 'P' ? -1 : 1;
            case '-':
                return b === 'X' ? -1 : 1;
            default:
                return b === 'P' ? -1 : b === '+' ? -1 : b === '-' ? -1 : 0;
        }
    });

    // dictionary
    // gather

    return particles;
}


// method-2
function sortParticles3(particles: Particle[]): Particle[] {

    const particleOrder: Particle[] = ['+', 'X', '-', 'P'];

    return particles.sort((a, b) => particleOrder.indexOf(a) - particleOrder.indexOf(b));
}



// problem - 3b
// solution
type Particle3b = '+' | '-' | 'P';

function sortParticle(particles: Particle3b[]): Particle3b[] {
    const indexOfP = particles.indexOf('P');

    // divide array
    const beforeP = particles.slice(0, indexOfP);
    const afterP = particles.slice(indexOfP + 1);

    // sort array
    const sortedBeforeP = beforeP.sort((a, b) => (a === '-' ? 1 : -1));
    const sortedAfterP = afterP.sort((a, b) => (a === '+' ? 1 : -1));

    // concat
    return [...sortedBeforeP, 'P', ...sortedAfterP];
}

console.log(sortParticle(['-', '+', '-', 'P', '+', '-', '+']));