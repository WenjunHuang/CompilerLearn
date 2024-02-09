const TypeEnvironment = require("./TypeEnvironment");

class Type {
    constructor(name) {
        this.name = name;
    }

    getName() {
        return this.name;
    }

    toString() {
        return this.getName();
    }

    equals(other) {
        if (other instanceof Type.Alias) {
            return other.equals(this);
        }

        if (other instanceof Type.Union) {
            return other.equals(this);
        }

        return this.getName() === other.getName();
    }

    static fromString(typeStr) {
        if (this.hasOwnProperty(typeStr)) {
            return this[typeStr];
        }

        // Functions
        if (typeStr.includes('Fn<')) {
            return Type.Function.fromString(typeStr);
        }

        throw `Unknown type: ${typeStr}`;
    }
}

// Number type
Type.number = new Type("number");
// String type
Type.string = new Type("string");
// Boolean type
Type.boolean = new Type('boolean');
// Null type
Type.null = new Type('null');
// Any type
Type.any = new Type('any');

Type.Function = class extends Type {
    constructor({ name = null, paramTypes, returnType }) {
        super(name);
        this.paramTypes = paramTypes;
        this.returnType = returnType;
        this.name = this.getName();
    }

    /**
     * Returns name: Fn<returnType<p1,p2,...>>
     * Fn<number>
     * Fn<number<number,number>>
     */
    getName() {
        if (this.name == null) {
            const name = ['Fn<', this.returnType.getName()];

            // Params
            if (this.paramTypes.length !== 0) {
                const params = [];
                for (let i = 0; i < this.paramTypes.length; i++) {
                    params.push(this.paramTypes[i].getName());
                }
                name.push('<', params.join(','), '>');
            }
            name.push('>')
            this.name = name.join('');
        }
        return this.name;
    }

    equals(other) {
        if (this.paramTypes.length !== other.paramTypes.length) {
            return false;
        }

        // Same params
        for (let i = 0; i < this.paramTypes.length; i++) {
            if (!this.paramTypes[i].equals(other.paramTypes[i])) {
                return false;
            }
        }

        // Return type.
        if (!this.returnType.equals(other.returnType)) {
            return false;
        }

        return true;
    }

    static fromString(typeStr) {
        // Already compiled.
        if (Type.hasOwnProperty(typeStr)) {
            return Type[typeStr];
        }

        // Function type with return and params:
        let matched = /^Fn<(\w+)<([a-z,\s]+)>>$/.exec(typeStr);
        if (matched != null) {
            const [_, returnTypeStr, paramsString] = matched;

            // Parm types:
            const paramTypes = paramsString.split(/,\s*/g)
                .map(param => Type.fromString(param));

            return (Type[typeStr] = new Type.Function({ name: typeStr, paramTypes, returnType: Type.fromString(returnTypeStr) }));
        }

        // Function type with return type only:
        matched = /^Fn<(\w+)>$/.exec(typeStr);

        if (matched != null) {
            const [_, returnTypeStr] = matched;
            return (Type[typeStr] = new Type.Function({
                name: typeStr,
                paramTypes: [],
                returnType: Type.fromString(returnTypeStr),
            }));
        }
    }
}

Type.Alias = class extends Type {
    constructor({ name, parent }) {
        super(name);
        this.parent = parent;
    }

    equals(other) {
        if (this.getName() === other.getName()) {
            return true;
        }
        return this.parent.equals(other);
    }
}

/**
 * Class type: (class ...)
 * 
 * Creates a new TypeEnvironment
 */
Type.Class = class extends Type {
    constructor({ name, superClass = Type.null }) {
        super(name);
        this.superClass = superClass;
        this.env = new TypeEnvironment({}, superClass != Type.null ? superClass.env : null);
    }

    /** Returns field type. */
    getField(name) {
        return this.env.lookup(name);
    }

    equals(other) {
        if (this === other)
            return true;

        // Aliases:
        if (other instanceof Type.Alias) {
            return other.equals(this);
        }

        // Super class for inheritance:
        if (this.superClass != Type.null) {
            return this.superClass.equals(other);
        }

        // Anything else:
        return false
    }
}

// Union type: (or string number)
Type.Union = class extends Type {
    constructor({ name, optionTypes }) {
        super(name);
        this.optionTypes = optionTypes;
    }

    // Whether this union includes all types.
    includesAll(types) {
        if (types.length !== this.optionTypes.length) {
            return false;
        }

        for (const type_ of types) {
            if (!this.equals(type_)) {
                return false;
            }
        }

        return true;
    }

    equals(other) {
        if (this === other) {
            return true;
        }

        // Aliases:
        if (other instanceof Type.Alias) {
            return other.equals(this);
        }

        // Other union:
        if (other instanceof Type.Union) {
            return this.includesAll(other.optionTypes);
        }

        // Anything else:
        return this.optionTypes.some(t => t.equals(other));
    }
}

module.exports = Type