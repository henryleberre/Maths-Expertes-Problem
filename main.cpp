#include <cmath>
#include <math.h>
#include <mutex>
#include <stack>
#include <array>
#include <limits>
#include <future>
#include <string>
#include <vector>
#include <bitset>
#include <thread>
#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <numeric>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <immintrin.h>

std::mutex sPrintMutex;

#define LOCK_PRINT() const std::lock_guard<std::mutex> lg(sPrintMutex)

constexpr float         K_EPSILON         = 0.01f;
constexpr float         K_ERROR           = std::numeric_limits<float>::lowest();
constexpr std::uint64_t K_MAX_FACTORIELLE = 3000;
constexpr std::uint64_t K_BITS_PAR_OCTET  = 8;
constexpr std::uint64_t K_NOMBRE_THREADS  = 8;

template <typename T, size_t N>
struct FixedStack {
    std::array<T, N> mem;
    T* pTop = mem.data();

    constexpr inline size_t Capacity() const noexcept { return N;                 }
    constexpr inline size_t Size()     const noexcept { return pTop - mem.data(); }

    inline void Push(const T& data) noexcept { *(pTop++) = data; }
    inline T    Pop()               noexcept { return *(--pTop); }
}; // struct FixedStack

inline float logarithme(const float a, const float b) noexcept { return std::log(b) / std::log(a); }

std::uint64_t factorielle(const std::uint64_t x) noexcept {
    if (x == 0) { return 1; }

    return x * factorielle(x - 1);
}

enum class Operation : std::uint8_t {
    ADDITIONNER = 0b000,
    SOUSTRAIRE  = 0b001,
    MULTIPLIER  = 0b010,
    DIVISER     = 0b011,
    PUISSANCE   = 0b100,
    LOGARITHME  = 0b101
}; // enum class Operation

struct Operateur {
    union {
        struct {
            std::uint8_t operation                 : 3;
            std::uint8_t factorielleOperandeGauche : 2;
            std::uint8_t factorielleOperandeDroit  : 2;
            std::uint8_t ajouterProchainNumero     : 1;
        };
        std::uint8_t encodage;
    };

    [[nodiscard]] inline float operator()(float operandeGauche, float operandeDroit) const noexcept {
        for (int i = 0; i < this->factorielleOperandeGauche; ++i) {
            if ((operandeGauche - std::floor(operandeGauche)) > K_EPSILON || !std::isnormal(operandeGauche) || operandeGauche < 0.f || operandeGauche > K_MAX_FACTORIELLE)
                return K_ERROR;

            operandeGauche = static_cast<float>(factorielle(static_cast<std::uint64_t>(operandeGauche)));
        }

        for (int i = 0; i < this->factorielleOperandeDroit; ++i) {
            if ((operandeDroit - std::floor(operandeDroit)) > K_EPSILON || !std::isnormal(operandeDroit) || operandeDroit < 0.f || operandeDroit > K_MAX_FACTORIELLE)
                return K_ERROR;

            operandeDroit = static_cast<float>(factorielle(static_cast<std::uint64_t>(operandeDroit)));
        }

        switch (static_cast<Operation>(this->operation)) {
        case Operation::ADDITIONNER: return operandeGauche + operandeDroit;
        case Operation::SOUSTRAIRE:  return operandeGauche - operandeDroit;
        case Operation::MULTIPLIER:  return operandeGauche * operandeDroit;
        case Operation::DIVISER:     return operandeGauche / operandeDroit;
        case Operation::PUISSANCE:   return std::pow(operandeGauche, operandeDroit);
        case Operation::LOGARITHME:
            if (operandeGauche <= 0.f)
                return K_ERROR;
            return logarithme(operandeGauche, operandeDroit);
        }

        return K_ERROR;
    }

    std::string to_string(std::string operandeGauche, std::string operandeDroit) const noexcept {
        for (int i = 0; i < this->factorielleOperandeGauche; ++i) { operandeGauche = '('+operandeGauche + "!)"; }
        for (int i = 0; i < this->factorielleOperandeDroit;  ++i) { operandeDroit  = '('+operandeDroit  + "!)"; }

        switch (static_cast<Operation>(this->operation)) {
        case Operation::ADDITIONNER: return '(' + operandeGauche + '+' + operandeDroit + ')';
        case Operation::SOUSTRAIRE:  return '(' + operandeGauche + '-' + operandeDroit + ')';
        case Operation::MULTIPLIER:  return '(' + operandeGauche + '*' + operandeDroit + ')';
        case Operation::DIVISER:     return '(' + operandeGauche + '/' + operandeDroit + ')';
        case Operation::PUISSANCE:   return "((" + operandeGauche + ")^(" + operandeDroit + "))";
        case Operation::LOGARITHME:  return "log_{" + operandeGauche + "}(" + operandeDroit + ")";
        }

        return "ERROR";
    }

    [[nodiscard]] static inline std::uint64_t ObtenirEncodageDebut() noexcept {
        return 0;
    }

    [[nodiscard]] static inline std::uint64_t ObtenirEncodageFin() noexcept {
        return static_cast<uint64_t>(pow(2, sizeof(Operateur::encodage) * K_BITS_PAR_OCTET));
    }
}; // struct Operateur

struct SequencesOperateurs {
    union {
        std::array<Operateur, 8> operateurs;
        std::uint64_t            encodage;
    };

    [[nodiscard]] inline const Operateur& ObtenirOperateur(const std::uint8_t& indiceOperateur) const noexcept {
        return this->operateurs[indiceOperateur];
    }

    [[nodiscard]] static inline std::uint64_t ObtenirEncodageDebut(const std::uint8_t& nombreOperateurs) noexcept {
        return 0;
        //        return (nombreOperateurs == 0) ? 0 : ObtenirEncodageFin(nombreOperateurs - 1) + 1;
    }

    [[nodiscard]] static inline std::uint64_t ObtenirEncodageFin(const std::uint8_t& nombreOperateurs) noexcept {
        return static_cast<std::uint64_t>(std::pow(2, sizeof(Operateur::encodage) * K_BITS_PAR_OCTET * nombreOperateurs));
    }
}; // struct Operateurs

// TODO: optimisation: dernier bit est toujours 0
struct CombinationsOperandes {
    std::uint8_t encodage;

    [[nodiscard]] inline std::uint64_t ObtenirOperande(const std::uint8_t& indiceOperande) const noexcept {
        std::uint8_t  encodageTmp = this->encodage;
        std::uint64_t currentDigit   = 1;
        std::uint64_t currentOperand = 0;
        std::uint64_t currentOperandIndex = 0;
        while (currentOperandIndex != indiceOperande + 1) {
            if (encodageTmp & 1) {
                currentOperand = 0;
                while (encodageTmp & 1) {
                    currentOperand = currentOperand * 10 + currentDigit++;
                    encodageTmp >>= 1ul;
                }
                currentOperand = currentOperand * 10 + currentDigit++;
                encodageTmp >>= 1ul;
            } else {
                currentOperand = currentDigit++;
                encodageTmp >>= 1ul;
            }
            currentOperandIndex++;
        }

        return currentOperand;
    }

    [[nodiscard]] static inline std::uint64_t ObtenirEncodageDebut(const std::uint8_t& nombreOperandes) {
        return 0;
        // return (nombreOperandes == 1) ? 0 : ObtenirEncodageFin(nombreOperandes - 1) + 1;
    }

    [[nodiscard]] static inline std::uint64_t ObtenirEncodageFin(const std::uint8_t& nombreOperandes) {
        return static_cast<std::uint64_t>(std::pow(2, nombreOperandes-1)); // last bit is always 0
    }
}; // struct PermutationsOperandes 

struct PolariteOperandes {
    std::uint8_t encodage;

    [[nodiscard]] inline float ObtenirPolaritee(const std::uint64_t& indiceOperande) const noexcept {
        return (this->encodage & (1ul << indiceOperande) ? -1.f : 1.f);
    }

    [[nodiscard]] static inline std::uint64_t ObtenirEncodageDebut(const std::uint8_t& nombreOperandes) {
        return 0;
    }

    [[nodiscard]] static inline std::uint64_t ObtenirEncodageFin(const std::uint8_t& nombreOperandes) {
        return static_cast<std::uint64_t>(std::pow(2, nombreOperandes)) - 1;
    }
};

struct ExpressionRPN {
    SequencesOperateurs   operateurs;
    CombinationsOperandes combinations;
    PolariteOperandes     polaritees;

    std::uint8_t nombreOperandes;
    std::uint8_t longueur;

    [[nodiscard]] inline float ProchaineOperandeSequence(const std::uint8_t indiceOperande) const noexcept {
        return this->polaritees.ObtenirPolaritee(indiceOperande) * this->combinations.ObtenirOperande(indiceOperande);
    }

    [[nodiscard]] inline float operator()() const noexcept {
        FixedStack<float, 50> stack;
        stack.Push(this->ProchaineOperandeSequence(0));
        stack.Push(this->ProchaineOperandeSequence(1));

        std::uint8_t operandIndex = 1;
        for (int i = 0; i < this->nombreOperandes - 1; i++) {
            const Operateur& operateur = this->operateurs.ObtenirOperateur(i);

            if (i != 0) {
                if (operateur.ajouterProchainNumero) {
                    if (operandIndex - 1 <= this->nombreOperandes) { stack.Push(this->ProchaineOperandeSequence(++operandIndex)); }
                    else { return K_ERROR; }
                    if (operandIndex - 1 <= this->nombreOperandes) { stack.Push(this->ProchaineOperandeSequence(++operandIndex)); }
                    else { return K_ERROR; }
                }
                else if (stack.Size() < 2) {
                    if (operandIndex - 1 <= this->nombreOperandes) { stack.Push(this->ProchaineOperandeSequence(++operandIndex)); }
                    else { return K_ERROR; }
                }
            }

            const float operandeDroit = stack.Pop();
            const float operandeGauche = stack.Pop();

            const float resultOperation = operateur(operandeGauche, operandeDroit);

            if (resultOperation == K_ERROR)
                return K_ERROR;

            stack.Push(resultOperation);
        }

        if (stack.Size() != 1) { return K_ERROR; }

        return stack.Pop();
    }

    std::string to_string() const noexcept {
        FixedStack<std::string, 50> stack;
        stack.Push('('+std::to_string((int)this->ProchaineOperandeSequence(0))+')');
        stack.Push('('+std::to_string((int)this->ProchaineOperandeSequence(1))+')');

        std::uint8_t operandIndex = 1;
        for (int i = 0; i < this->nombreOperandes - 1; i++) {
            const Operateur& operateur = this->operateurs.ObtenirOperateur(i);

            if (i != 0) {
                if (operateur.ajouterProchainNumero) {
                    if (operandIndex - 1 <= this->nombreOperandes) { stack.Push('('+std::to_string((int)this->ProchaineOperandeSequence(++operandIndex))+')'); }
                    else { return "ERROR"; }
                    if (operandIndex - 1 <= this->nombreOperandes) { stack.Push('('+std::to_string((int)this->ProchaineOperandeSequence(++operandIndex))+')'); }
                    else { return "ERROR"; }
                }
                else if (stack.Size() < 2) {
                    if (operandIndex - 1 <= this->nombreOperandes) { stack.Push('('+std::to_string((int)this->ProchaineOperandeSequence(++operandIndex))+')'); }
                    else { return "ERROR"; }
                }
            }

            const std::string operandeDroit  = stack.Pop();
            const std::string operandeGauche = stack.Pop();

            const std::string resultOperation = operateur.to_string(operandeGauche, operandeDroit);

            if (resultOperation == "ERROR")
                return "ERROR";

            stack.Push(resultOperation);
        }

        if (stack.Size() != 1) { return "ERROR"; }

        return stack.Pop();
    }
}; // struct ExpressionRPN

void ThreadWorker(ExpressionRPN expr, const std::uint64_t operateursEncodageDebut, const std::uint64_t operateursEncodageFin) noexcept {
    for (expr.operateurs.encodage = operateursEncodageDebut; expr.operateurs.encodage <= operateursEncodageFin; ++expr.operateurs.encodage) {
        const float result = expr();

        if (result >= 2021 && result <= 2500 && (result - std::floor(result)) <= K_EPSILON) {
            LOCK_PRINT();

            if (expr.combinations.encodage != 0) std::cout << "Fusion: ";

            std::cout << expr.to_string() << '=' << result << ". n=" << (int)expr.longueur << "\n";
        }
    }
}

void DispatchThreadWork(ExpressionRPN expr) noexcept {
    std::array<std::future<void>, K_NOMBRE_THREADS> threadFutures;

    const std::uint64_t seqEncodagesDebut    = SequencesOperateurs::ObtenirEncodageDebut(expr.nombreOperandes - 1);
    const std::uint64_t seqEncodagesFin      = SequencesOperateurs::ObtenirEncodageFin  (expr.nombreOperandes - 1);
    const std::uint64_t seqEncdagesParThread = static_cast<std::uint64_t>((seqEncodagesFin - seqEncodagesDebut) / static_cast<float>(K_NOMBRE_THREADS));
    
    std::uint64_t encodageDebutCourant = seqEncodagesDebut;
    for (int i = 0; i < K_NOMBRE_THREADS; ++i) {
        if (i == K_NOMBRE_THREADS - 1) {
            threadFutures[i] = std::async(std::launch::async, ThreadWorker, expr, encodageDebutCourant, seqEncodagesFin);
        } else {
            threadFutures[i] = std::async(std::launch::async, ThreadWorker, expr, encodageDebutCourant, encodageDebutCourant + seqEncdagesParThread);
            encodageDebutCourant += seqEncdagesParThread + 1;
        }
    }

    for (std::future<void>& tf : threadFutures)
        tf.wait();
}

int main(int argc, char* argv[]) {
    setvbuf(stdout, NULL, _IONBF, 0);

    ExpressionRPN expr{};
    for (expr.longueur = 3; expr.longueur <= 8; ++expr.longueur) {
       // std::cout << "[LONGUEUR = " << (int)expr.longueur << "]\n";

        for (expr.combinations.encodage = CombinationsOperandes::ObtenirEncodageDebut(expr.longueur); expr.combinations.encodage <= CombinationsOperandes::ObtenirEncodageFin(expr.longueur); ++expr.combinations.encodage) {
           // std::cout << "[COMBINATIONS = " << (int)expr.combinations.encodage << "]\n";
            expr.nombreOperandes = expr.longueur - std::bitset<6>(expr.combinations.encodage & (0xFF >> (8-expr.longueur+2))).count();

            for (expr.polaritees.encodage = PolariteOperandes::ObtenirEncodageDebut(expr.nombreOperandes); expr.polaritees.encodage <= PolariteOperandes::ObtenirEncodageFin(expr.nombreOperandes); ++expr.polaritees.encodage) {
               // std::cout << "[POLARITIES = " << (int)expr.polaritees.encodage << "]\n";

                DispatchThreadWork(expr);
            }
        }
    }
}