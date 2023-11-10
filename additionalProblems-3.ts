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

    return particles;
}


// method-2
function sortParticles3(particles: Particle[]): Particle[] {

    const particleOrder: Particle[] = ['+', 'X', '-', 'P'];

    return particles.sort((a, b) => particleOrder.indexOf(a) - particleOrder.indexOf(b));
}

// problem - 3b
// method-1
function sortParticlesB(particles: Particle[]): Particle[] {

    return particles.sort((a, b) => (a === 'P' ? 1 : a === '-' ? -1 : 0) - (b === 'P' ? 1 : b === '-' ? -1 : 0));
}

//   method-2
function sortParticlesB2(particles: Particle[]): Particle[] {
    const particleOrder: Particle[] = ['+', '-', 'P'];

    return particles.sort((a, b) => particleOrder.indexOf(a) - particleOrder.indexOf(b));
}