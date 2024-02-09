
const Type = require("./Type");
const TypeEnvironment = require("./TypeEnvironment");

class EvaTC {
    constructor() {
        this.global = this._createGlobal();
    }

    _createGlobal() {
        return new TypeEnvironment({
            VERSION: Type.string,
            sum: Type.fromString('Fn<number<number,number>>'),
            square: Type.fromString('Fn<number<number>>'),
            typeof: Type.fromString('Fn<string<any>>'),
        });
    }

    /**
     * Evaluates global code wrapping into a block.
     */
    tcGlobal(exp) {
        return this._tcBody(exp, this.global);
    }

    /**
     * Checks body (global or function)
     */
    _tcBody(body, env) {
        if (body[0] === 'begin') {
            return this._tcBlock(body, env);
        }
        return this.tc(body, env);
    }

    tc(exp, env = this.global) {
        if (this._isNumber(exp)) {
            return Type.number;
        }

        if (this._isString(exp)) {
            return Type.string;
        }

        if (this._isBoolean(exp)) {
            return Type.boolean;
        }

        // Variable access: foo
        if (this._isVariableName(exp)) {
            return env.lookup(exp);
        }

        if (this._isBinary(exp)) {
            return this._binary(exp, env);
        }

        if (this._isBooleanBinary(exp)) {
            return this._booleanBinary(exp, env);
        }

        // Type declaration/alias: (type <namew> <base>) 
        if (exp[0] === 'type') {
            const [_tag, name, base] = exp;

            // Union type: (or number string)
            if (Array.isArray(base) && base[0] === 'or') {
                const options = base.slice(1);
                const optionTypes = options.map(option => Type.fromString(option));
                return (Type[name] = new Type.Union({ name, optionTypes }));
            }


            // Type alias:
            if (Type.hasOwnProperty(name)) {
                throw `Type ${name} is already defined: ${Type[name]}`;
            }

            if (!Type.hasOwnProperty(base)) {
                throw `Type ${base} is not defined.`;
            }

            return (Type[name] = new Type.Alias({ name, parent: Type[base] }));
        }

        // Class declaration: (class <Name> <Super> <Body>)
        if (exp[0] === 'class') {
            const [_tag, name, superClassName, body] = exp;

            // Resolve super:
            const superClass = Type[superClassName];

            // New class (type):
            const classType = new Type.Class({ name, superClass });

            // Class is accessible by name.
            Type[name] = env.define(name, classType);

            // Body is evaluated in the class environment.
            this._tcBody(body, classType.env);
            return classType;
        }

        // Class instantiation: (new <Class> <Arguments>...)
        if (exp[0] === 'new') {
            const [_tag, className, ...argValues] = exp;
            const classType = Type[className];

            if (classType == null) {
                throw `Unknown class ${className}.`;
            }

            const argTypes = argValues.map(arg => this.tc(arg, env));

            return this._checkFunctionCall(
                classType.getField('constructor'),
                [classType, ...argTypes],
                env,
                exp
            );
        }

        // Super expressions: (super <ClassName>)
        if (exp[0] === 'super') {
            const [_tag, className] = exp;

            const classType = Type[className];

            if (classType == null) {
                throw `Unknown class ${className}.`;
            }
            return classType.superClass;
        }

        // Property access: (prop <Object> <Name>)
        if (exp[0] === 'prop') {
            const [_tag, instance, name] = exp;

            const instanceType = this.tc(instance, env);
            return instanceType.getField(name);
        }

        // Variable declaration: (var x 10)
        // with typecheck : (var (x number) "foo") // error
        if (exp[0] === 'var') {
            const [_tag, name, value] = exp;

            // Infer actual type:
            const valueType = this.tc(value, env);

            // With type check:
            if (Array.isArray(name)) {
                const [varName, typeStr] = name;
                const expectedType = Type.fromString(typeStr);

                // Check the type:
                this._expect(valueType, expectedType, value, exp);

                return env.define(varName, expectedType);
            }

            // Simple name:
            return env.define(name, valueType);
        }



        // Variable update: (set x 10)
        if (exp[0] === 'set') {
            const [_tag, ref, value] = exp;

            // 1. Assignment to a property: (set (prop <instance> <propName>) <value>)
            if (Array.isArray(ref) && ref[0] === 'prop') {
                const [_tag, instance, propName] = ref;
                const instanceType = this.tc(instance, env);
                const valueType = this.tc(value, env);
                const propType = instanceType.getField(propName);

                return this._expect(valueType, propType, value, exp);
            }

            // The type of the new value should match to the
            // previous type when the variable was defined:
            const valueType = this.tc(value, env);
            const varType = this.tc(ref, env);
            return this._expect(valueType, varType, value, exp);
        }

        // Block: sequence of expressions
        if (exp[0] === 'begin') {
            const blockEnv = new TypeEnvironment({}, env);
            return this._tcBlock(exp, blockEnv);
        }

        // if-expression:
        if (exp[0] === 'if') {
            const [_tag, condition, consequent, alternate] = exp

            const t1 = this.tc(condition, env);
            this._expect(t1, Type.boolean, condition, exp);

            // Initially, environment used to tc consequent part
            // is the same as the main env, however can be updated
            // for the union type with type casting:
            let consequentEnv = env;


            // Check if the condition is a type casting rule.
            // This is used with union types to make a type concrete:
            // (if (== (typeof foo) "string") ...)
            if (this._isTypeCastCondition(condition)) {
                const [name, specificType] = this._getSpecifiedType(condition);

                // update environment with the concrete type for this name:
                consequentEnv = new TypeEnvironment({ [name]: Type.fromString(specificType) }, env,);
            }

            const t2 = this.tc(consequent, consequentEnv);
            const t3 = this.tc(alternate, env);

            // Same types for both branches:
            return this._expect(t3, t2, exp, exp);
        }

        // while-expression:
        if (exp[0] === 'while') {
            const [_tag, condition, body] = exp;

            // Boolean condition:
            const t1 = this.tc(condition, env);
            this._expect(Type.boolean, t1, condition, exp);

            return this.tc(body, env);
        }

        // Function declaration: (def square ((x number)) -> number (* x x))
        // Syntactic sugar for : (var square (lambda ((x number)) -> number (* x x)))
        if (exp[0] === 'def') {
            // Transpile to a variable declaration:
            const varExp = this._transformDefToVarLambda(exp);
            const name = exp[1];
            const params = exp[2];
            const returnTypeStr = exp[4];
            const body = exp[5];

            // To support recursive function calls, we must predefine the function with the signature:
            const paramTypes = params.map(([name, typeStr]) =>
                Type.fromString(typeStr));

            env.define(name,
                new Type.Function({
                    paramTypes,
                    returnType: Type.fromString(returnTypeStr),
                }));

            // Actually validate the body
            return this._tcFunction(params, returnTypeStr, body, env);
        }

        // Lambda function:
        if (exp[0] === 'lambda') {
            const [_tag, params, _retDecl, returnTypeStr, body] = exp;
            return this._tcFunction(params, returnTypeStr, body, env);
        }

        // Function calls
        // (square 2)
        if (Array.isArray(exp)) {
            const fn = this.tc(exp[0], env);
            const argValues = exp.slice(1);

            // Passed arguments:
            const argTypes = argValues.map(arg => this.tc(arg, env))

            return this._checkFunctionCall(fn, argTypes, env, exp);
        }

        throw `Unknown type for expression ${exp}.`;
    }

    /**
     * Whether the if-condition is type casting/specification.
     * This is used with union types to make a type concrete:
     * (if (== (typeof foo) "string") ...)
     */
    _isTypeCastCondition(condition) {
        const [op, lhs] = condition;
        return op === '==' && lhs[0] === 'typeof';
    }

    /**
     * Returns specific type after casting.
     */
    _getSpecifiedType(condition) {
        const [_op, [_typeof, name], specificType] = condition;

        // Return name and the new type (stripping quotes).
        return [name, specificType.slice(1, -1)];
    }

    /**
     * Whether the function is generic.
     * 
     * (def foo <K> ((x K)) -> K (+ x x)) 
     */
    _isGenericDefFunction(exp) {
        return exp.length === 7 && /^<[^>]+>$/.test(exp[2]);
    }

    _transformDefToVarLambda(exp) {
        // 1. Generic function:
        if (this._isGenericDefFunction(exp)) {
            const [_tag, name, genericTypesStr, params, _retDecl, returnTypeStr, body] = exp;
            return ['var', name, ['lambda', genericTypesStr, params, _retDecl, returnTypeStr, body]];
        }

        // 2. Simple function:
        const [_tag, name, params, _retDel, returnTypeStr, body] = exp;
        return ['var', name, ['lambda', params, _retDel, returnTypeStr, body]];
    }

    /**
     * Checks function call.
     */
    _checkFunctionCall(fn, argTypes, env, exp) {
        // Check arity;
        if (fn.paramTypes.length !== argTypes.length) {
            throw `\nFunction ${exp[0]} ${fn.getName()} expects ${fn.paramTypes.legth} arguments, ${argTypes.length} given in ${exp}.\n`;
        }

        // Check if argument types match the parameter types:
        argTypes.forEach((argType, index) => {
            if (fn.paramTypes[index] === Type.any) {
                return;
            }
            this._expect(argType, fn.paramTypes[index], argTypes[index], exp);
        });

        return fn.returnType;
    }

    /**
     * Checks function body. 
     */
    _tcFunction(params, returnTypeStr, body, env) {
        const returnType = Type.fromString(returnTypeStr);

        // Parameters environment and types:
        const paramsRecord = {};
        const paramTypes = [];

        params.forEach(([name, typeStr]) => {
            const paramType = Type.fromString(typeStr);
            paramsRecord[name] = paramType;
            paramTypes.push(paramType);
        });

        const fnEnv = new TypeEnvironment(paramsRecord, env);

        // Check the body in the extended environment:
        const actualReturnType = this._tcBody(body, fnEnv);

        // Check return type:
        if (!returnType.equals(actualReturnType)) {
            throw `Expected function ${body} to return ${returnType}, but got ${actualReturnType}.`;
        }

        return new Type.Function({
            paramTypes,
            returnType,
        });
    }

    /**
     * Checks a block. 
     */
    _tcBlock(block, env) {
        let result;
        const [_tag, ...expressions] = block;

        expressions.forEach(exp => {
            result = this.tc(exp, env);
        });
        return result;
    }

    /**
     * Whether the expression is boolean binary. 
     */
    _isBooleanBinary(exp) {
        return ['<', '>', '<=', '>=', '==', '!='].includes(exp[0]);
    }

    _booleanBinary(exp, env) {
        this._checkArity(exp, 2);

        const t1 = this.tc(exp[1], env);
        const t2 = this.tc(exp[2], env);

        this._expect(t2, t1, exp[2], exp);
        return Type.boolean;
    }

    _isVariableName(exp) {
        return typeof exp === 'string' && /^[+\-*/<>=a-zA-Z0-9_:]+$/.test(exp);
    }

    _isBinary(exp) {
        return /^[+\-*/]$/.test(exp[0]);
    }

    _binary(exp, env) {
        this._checkArity(exp, 2);

        const t1 = this.tc(exp[1], env);
        const t2 = this.tc(exp[2], env);

        const allowedTypes = this._getOperandTypesForOperator(exp[0]);
        this._expectOperatorType(t1, allowedTypes, exp);
        this._expectOperatorType(t2, allowedTypes, exp);

        return this._expect(t2, t1, exp[2], exp);
    }

    _getOperandTypesForOperator(operator) {
        switch (operator) {
            case '+':
                return [Type.string, Type.number];
            case '-':
                return [Type.number];
            case '*':
                return [Type.number];
            case '/':
                return [Type.number];
            default:
                throw `Unknown operator: ${operator}.`
        }
    }

    // Throws if operator type doesn't expect the operand.
    _expectOperatorType(type_, allowedTypes, exp) {
        // For union type, _all_ sub-types should suppot this operation:
        if (type_ instanceof Type.Union) {
            if (type_.includesAll(allowedTypes)) {
                return;
            }
        } else {
            if (allowedTypes.some(t => t.equals(type_))) {
                return;
            }
        }
        throw `\nUnexpected type: ${type_} in ${exp}, allowed: ${allowedTypes}`;
    }
    _throw(actualType, expectedType, value, exp) {
        throw `\nExpected "${expectedType}" type for ${value} in ${exp}, but got "${actualType}" type.\n`;

    }

    _expect(actualType, expectedType, value, exp) {
        if (!actualType.equals(expectedType)) {
            this._throw(actualType, expectedType, value, exp);
        }

        return actualType;
    }

    _checkArity(exp, arity) {
        if (exp.length - 1 !== arity) {
            throw `\nOperator '${exp[0]}' expects ${arity} operands, ${exp.length - 1} given in ${exp}.\n`
        }
    }

    _isBoolean(exp) {
        return typeof exp === 'boolean' || exp === 'true' || exp === 'false';
    }
    _isNumber(exp) {
        return typeof exp === 'number';
    }

    _isString(exp) {
        return typeof exp === 'string' && exp[0] === '"' && exp.slice(-1) === '"';
    }
}

module.exports = EvaTC;