/**
 * Generates a character class for digits between a and b inclusive.
 * @param {number} a - Starting digit.
 * @param {number} b - Ending digit.
 * @returns {string} - A string representing the digit range in regex.
 */
function range(a, b) {
    return a === b ? `${a}` : `[${a}-${b}]`;
}

/**
 * Generates a regex that matches numbers smaller than the given number string.
 * @param {string} numberStr - The number as a string.
 * @returns {string} - A regex pattern string.
 */
function smallerThan(numberStr) {
    if (numberStr.length === 0) {
        return "";
    }
    const firstDigit = parseInt(numberStr.charAt(0), 10);
    const restOfNumber = numberStr.substring(1);

    const rangePattern = firstDigit > 0 ? range(0, firstDigit - 1) : "";
    const recursivePattern = `(${numberStr.charAt(0)}(${smallerThan(restOfNumber)}))`;
    return rangePattern ? `((${rangePattern}\\d*)|${recursivePattern})` : recursivePattern;
}

/**
 * Generates a regex that matches numbers greater than the given number string.
 * @param {string} numberStr - The number as a string.
 * @returns {string} - A regex pattern string.
 */
function greaterThan(numberStr) {
    if (numberStr.length === 0) {
        return "";
    }
    const firstDigit = parseInt(numberStr.charAt(0), 10);
    const restOfNumber = numberStr.substring(1);

    const rangePattern = firstDigit < 9 ? range(firstDigit + 1, 9) : "";
    const recursivePattern = `(${numberStr.charAt(0)}(${greaterThan(restOfNumber)}))`;
    return rangePattern ? `((${rangePattern}\\d*)|${recursivePattern})` : recursivePattern;
}

/**
 * Generates a regex that matches numbers between two given number strings.
 * @param {string} startStr - The starting number as a string.
 * @param {string} endStr - The ending number as a string.
 * @returns {string} - A regex pattern string.
 */
function between(startStr, endStr) {
    if (startStr.length === 0 && endStr.length === 0) {
        return "";
    } else if (startStr.length === 0) {
        return smallerThan(endStr);
    } else if (endStr.length === 0) {
        return greaterThan(startStr);
    } else if (startStr.charAt(0) === endStr.charAt(0)) {
        return `${startStr.charAt(0)}(${between(startStr.substring(1), endStr.substring(1))})`;
    } else if (startStr.charAt(0) > endStr.charAt(0)) {
        return "⊥"; // No numbers between if the start digit is greater than the end digit
    } else {
        const startFirstDigit = parseInt(startStr.charAt(0), 10);
        const endFirstDigit = parseInt(endStr.charAt(0), 10);

        const startPattern = `${startStr.charAt(0)}(${greaterThan(startStr.substring(1))})`;
        const middleRangeStart = startFirstDigit + 1;
        const middleRangeEnd = endFirstDigit - 1;
        const middlePattern = middleRangeEnd >= middleRangeStart ? `[${range(middleRangeStart, middleRangeEnd)}]\\d*` : "";
        const endPattern = `${endStr.charAt(0)}(${smallerThan(endStr.substring(1))})`;

        if (middlePattern) {
            return `((${startPattern})|(${middlePattern})|(${endPattern}))`;
        } else {
            return `((${startPattern})|(${endPattern}))`;
        }
    }
}

