
#include <string>
#include <istream>
class parseable {
    public:
    virtual bool parse(std::istream &in) = 0;
};

class jimple_statement : public parseable {

};

class jimple_declaration : public jimple_statement {
public:

  explicit jimple_declaration(std::istream &in) {
      parse(in);
  }

  virtual bool parse(std::istream &in) override;
  std::string type_name;
  std::string var_name;
};
