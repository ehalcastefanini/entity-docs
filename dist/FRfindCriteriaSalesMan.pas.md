<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é criar um componente de interface gráfica que permite aos usuários definir critérios de busca para vendedores (SalesMan). Ele fornece campos para entrada de descrição e seleção de escritório, que são usados para filtrar dados em um sistema. Este componente é útil em sistemas onde é necessário realizar buscas baseadas em critérios específicos.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do componente.
  - Componentes visuais como `TsLabel`, `TsEdit`, e `TFRAMEFindEditSOA` para a interface do usuário.
  - Manipulação de critérios de busca com a classe `TFieldCriteria`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `EDTdescription` (Campo de texto para descrição, tipo: `TsEdit`).
      - `FRAMEfindOffice` (Campo de seleção de escritório, tipo: `TFRAMEFindEditSOA`).
    - **Ações do Formulário e seus Efeitos:**
      - Preenchimento dos campos gera critérios de busca que podem ser usados para filtrar dados.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - O usuário pode preencher os campos de descrição e escritório para definir critérios de busca.
  - O sistema gera uma lista de critérios baseada nos valores preenchidos.

* **Componentes Principais:**
  - `EDTdescription`: Campo de entrada para a descrição.
  - `FRAMEfindOffice`: Componente para seleção de escritório.
  - `GetCriteriaValues`: Método que retorna os critérios de busca baseados nos valores preenchidos.

* **Tradução para Pseudo-código:**
  - Evento `OnChange` no campo `EDTdescription`: `se valor do campo mudar, validar entrada`.
  - Evento `OnChange` no campo `FRAMEfindOffice`: `se valor do campo mudar, validar entrada`.
  - Método `GetCriteriaValues`: `se campos preenchidos, gerar critérios de busca`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do componente com o método `Initialize`, que limpa os campos e configura o componente de escritório.
  - O usuário preenche os campos de descrição e escritório.
  - O método `GetCriteriaValues` é chamado para gerar os critérios de busca.

* **Dados Necessários:**
  - Descrição (campo de texto).
  - Escritório (campo de seleção).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão de busca (não especificado no código) deve estar habilitado apenas se pelo menos um dos campos estiver preenchido.

* **Filtros Disponíveis:**
  - Descrição: Permite busca por texto.
  - Escritório: Permite busca por seleção de escritório.

* **Mensagens de Erro:**
  - "Field Description must be defined" se o campo de descrição não for configurado.
  - "Field Office must be defined" se o campo de escritório não for configurado.

* **Valores Padrão dos Campos:**
  - `EDTdescription`: Valor inicial vazio.
  - `FRAMEfindOffice`: Valor inicial vazio.

* **Validação de Campos:**
  - `EDTdescription`: Deve ser preenchido em letras maiúsculas.
  - `FRAMEfindOffice`: Deve conter um valor válido.

---

## 5. Funções Principais:

* **`Initialize`:**
  - Limpa os campos e configura o componente de escritório.

* **`GetCriteriaValues`:**
  - Gera uma lista de critérios de busca baseada nos valores preenchidos nos campos.

* **`m_SetFindOffice`:**
  - Configura o componente de seleção de escritório.

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos especificadas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais especificados no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `TsLabel`, `TsEdit`, `TFRAMEFindEditSOA`: Componentes visuais para a interface do usuário.
  - `kneConfigObjects`: Para manipulação de objetos de configuração.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`: Componente para seleção de escritório.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `EDTdescription` (tipo: string, obrigatório, letras maiúsculas).
  - `FRAMEfindOffice` (tipo: string, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `EDTdescription` mapeado para `FFieldDescription`.
  - `FRAMEfindOffice` mapeado para `FFieldOffice`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  var
    Criteria: TArrayOfFieldCriteria;
  begin
    FRAMEfindCriteriaSalesMan.Initialize;
    FRAMEfindCriteriaSalesMan.FieldDescription := 'DescriptionField';
    FRAMEfindCriteriaSalesMan.FieldOffice := 'OfficeField';
    Criteria := FRAMEfindCriteriaSalesMan.GetCriteriaValues;
  end;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 525px; height: 34px;">
    <label for="description" style="position: absolute; left: 6px; top: 14px;">Description:</label>
    <input id="description" type="text" style="position: absolute; left: 66px; top: 10px; width: 152px;" />
    <label for="office" style="position: absolute; left: 224px; top: 13px;">Office:</label>
    <input id="office" type="text" style="position: absolute; left: 267px; top: 8px; width: 250px;" />
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* `Assert(FFieldDescription <> '', 'Field Description must be defined');`
  - Garante que o campo de descrição foi configurado.
* `Assert(FieldOffice <> '', 'Field Office must be defined');`
  - Garante que o campo de escritório foi configurado.

---

## 12. Conclusão:

O código fornece um componente reutilizável para definir critérios de busca baseados em descrição e escritório. Ele é bem estruturado e utiliza boas práticas, como validação de campos e inicialização adequada. No entanto, não há suporte explícito para integração com APIs ou manipulação de erros mais detalhada.

---

## 13. Resumo Curto:

Componente Delphi para definir critérios de busca por descrição e escritório, gerando filtros reutilizáveis. Inclui validação de campos e inicialização automática, sendo ideal para sistemas que requerem buscas personalizadas.#### **FRfindCriteriaSalesMan.pas**

```
unit FRfindCriteriaSalesMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRfindCriteria, sFrameAdapter, kneFRFindEditSOA, StdCtrls,
  sEdit, sLabel, kneConfigObjects, Buttons;

type
  TFRAMEfindCriteriaSalesMan = class(TFRAMEfindCriteria)
    LBL4: TsLabel;
    sLabel1: TsLabel;
    EDTdescription: TsEdit;
    FRAMEfindOffice: TFRAMEFindEditSOA;
  private
    FFieldDescription: string;
    FFieldOffice: string;
    { Private declarations }
    procedure m_SetFindOffice;
    procedure SetFieldDescription(const Value: string);
    procedure SetFieldOffice(const Value: string);
  protected
    { Potected declarations }
    function GetCriteriaValues: TArrayOfFieldCriteria; override;
  public
    { Public declarations }
    property FieldDescription: string read FFieldDescription write SetFieldDescription;
    property FieldOffice: string read FFieldOffice write SetFieldOffice;
    procedure Initialize; override;
    constructor Create(AOwner: TComponent); override;

  end;

var
  FRAMEfindCriteriaSalesMan: TFRAMEfindCriteriaSalesMan;

implementation

uses OfficeServiceUtils;

{$R *.dfm}

{ TFRAMEfindCriteriaCustomer }

constructor TFRAMEfindCriteriaSalesMan.Create(AOwner: TComponent);
begin
  inherited;

end;

function TFRAMEfindCriteriaSalesMan.GetCriteriaValues: TArrayOfFieldCriteria;
begin
  Result := nil;

  Assert(FFieldDescription <> '', 'Field Description must be defined');
  Assert(FieldOffice <> '', 'Field Office must be defined');

  // Description
  if EDTdescription.Text <> '' then
  begin
    // adicionar novo elemento
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := TFieldCriteria.Create;

    Result[High(Result)].logical  := 'AND'; // Operador l�gico: {AND,OR}
    Result[High(Result)].field    := FFieldDescription; // nome do campo do dataset associado
    Result[High(Result)].operator := 'MATCHES'; // Operador a usar: {<, <=, =, >=, >, <>, LIKE}
    Result[High(Result)].value    := EDTdescription.Text; // valor
  end;

  // Office
  if FRAMEfindOffice.Text <> '' then
  begin
    // adicionar novo elemento
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := TFieldCriteria.Create;

    Result[High(Result)].logical  := 'AND'; // Operador l�gico: {AND,OR}
    Result[High(Result)].field    := FFieldOffice; // nome do campo do dataset associado
    Result[High(Result)].operator := '='; // Operador a usar: {<, <=, =, >=, >, <>, LIKE}
    Result[High(Result)].value    := FRAMEfindOffice.Text; // valor
  end;
end;

procedure TFRAMEfindCriteriaSalesMan.Initialize;
begin
  inherited;

  m_SetFindOffice;

  EDTdescription.Text := '';
  FRAMEfindOffice.Clear;
end;

procedure TFRAMEfindCriteriaSalesMan.m_SetFindOffice;
begin
  with FRAMEfindOffice do
  begin
    // objecto configurador para FindEdit
```

#### **FRfindCriteriaSalesMan.dfm**

```
inherited FRAMEfindCriteriaSalesMan: TFRAMEfindCriteriaSalesMan
  Width = 525
  Height = 34
  object LBL4: TsLabel [0]
    Left = 224
    Top = 13
    Width = 31
    Height = 13
    Caption = 'O&ffice:'
    FocusControl = FRAMEfindOffice.FE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object sLabel1: TsLabel [1]
    Left = 6
    Top = 14
    Width = 56
    Height = 13
    Caption = 'Descri&ption:'
    FocusControl = EDTdescription
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object EDTdescription: TsEdit [2]
    Left = 66
    Top = 10
    Width = 152
    Height = 21
    CharCase = ecUpperCase
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  inline FRAMEfindOffice: TFRAMEFindEditSOA [3]
    Left = 267
    Top = 8
    Width = 250
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    inherited PNLdesc: TPanel
      Left = 73
      Width = 177
      inherited DBEDesc: TsDBEdit
        Width = 183
      end
      inherited EDDesc: TsEdit
        Width = 177
      end
    end
    inherited PNLcode: TPanel
      Width = 73
      inherited DBE: TsDBEdit
        Width = 52
      end
      inherited FE: TsMaskEdit
        Width = 52
      end
      inherited PNLbutton: TPanel
        Left = 52
      end
    end
  end
  inherited SKFAskin: TsFrameAdapter
    Left = 456
  end
end
```
<!-- tabs:end -->

