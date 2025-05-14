<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código `LpaymentCodes` implementa uma interface para exibir, gerenciar e pesquisar códigos de pagamento em um formato de lista. Ele permite que os usuários visualizem informações detalhadas sobre os códigos de pagamento, como código, descrição, desconto, data de vencimento, entre outros. Além disso, oferece funcionalidades para criar, modificar e visualizar registros, bem como realizar buscas avançadas.

* **Tecnologias Utilizadas:**
  - Delphi (VCL - Visual Component Library).
  - Componentes de interface gráfica como `TsLabel`, `TsDBText`, `TsCheckBox`, `TsPanel`, entre outros.
  - Manipulação de dados com `DBClient` e `DataSource`.
  - Ações e eventos gerenciados por `TActionList`.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `stat` (Status): Texto.
      - `paymentCode` (Código de Pagamento): Texto.
      - `descrip` (Descrição): Texto.
      - `discount` (Desconto): Numérico.
      - `dueDate` (Data de Vencimento): Data.
      - `discDate` (Data de Desconto): Data.
      - `FInvDisp` (Exibição de Fatura): Texto.
      - `FInvDispDesc` (Descrição da Exibição de Fatura): Texto.
    - **Ações da Grade e seus Efeitos:**
      - Ordenação de colunas.
      - Ocultação de campos.
      - Busca e filtragem de registros.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Criar um novo código de pagamento.
  - Modificar um código de pagamento existente.
  - Visualizar detalhes de um código de pagamento.
  - Realizar buscas simples e avançadas.
  - Filtrar registros por status ativo.

* **Componentes Principais:**
  - **Grade de Dados:** Exibe os registros de códigos de pagamento.
  - **Área de Busca:** Permite a entrada de critérios de busca.
  - **Botões de Ação:** Incluem botões para buscar, limpar critérios, criar, modificar e visualizar registros.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão de busca: `se botão buscar clicado então executar busca com critérios`.
  - Evento `OnClick` do botão de limpar critérios: `se botão limpar clicado então limpar todos os critérios de busca`.
  - Evento `OnChange` do checkbox "Ativo Apenas": `se checkbox alterado então atualizar filtro para mostrar apenas registros ativos`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário e configuração da grade (`GridSetup`).
  2. Configuração de eventos (`EventSetup`).
  3. Interação do usuário:
     - Usuário insere critérios de busca e clica no botão de busca.
     - Usuário pode criar, modificar ou visualizar registros clicando nos botões correspondentes.

* **Dados Necessários:**
  - Critérios de busca como código ou descrição.
  - Informações detalhadas para criar ou modificar um registro, como código, descrição, desconto, etc.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Buscar": Requer critérios de busca válidos.
  - Botão "Criar": Disponível sempre.
  - Botão "Modificar" e "Visualizar": Requer um registro selecionado.

* **Filtros Disponíveis:**
  - Filtro "Ativo Apenas" (checkbox).
  - Critérios de busca por código e descrição.

* **Mensagens de Erro:**
  - "Nenhum registro selecionado" ao tentar modificar ou visualizar sem selecionar um registro.
  - "Critérios de busca inválidos" se os critérios forem insuficientes.

* **Valores Padrão dos Campos:**
  - Checkbox "Ativo Apenas": Marcado por padrão.

* **Validações e Condições dos Campos:**
  - Campo "Código": Deve ser único e não vazio.
  - Campo "Descrição": Deve ter no máximo 255 caracteres.
  - Campo "Desconto": Deve ser numérico e maior ou igual a zero.

---

## 5. Funções Principais:

* **`CreateListForm`:** Cria e inicializa o formulário de lista.
* **`GridSetup`:** Configura a grade, incluindo campos ocultos e ordem de exibição.
* **`EventSetup`:** Configura os eventos associados ao formulário.
* **`CreateEditor`:** Cria o editor para modificar ou criar registros.

---

## 6. Consumo de Serviços de API:

* **Nenhuma chamada de API foi identificada no código fornecido.**

---

## 7. Campos Condicionais (Lógica do Formulário):

* Nenhum campo condicional foi identificado no código fornecido.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `sSkinProvider`, `sLabel`, `sBevel`, `sCheckBox`: Para estilização e componentes visuais.
  - `cxGrid`, `cxDBData`: Para exibição de dados em grade.

* **Componentes Personalizados:**
  - `TFORMkneCBListSOA`: Classe base para formulários de lista.
  - `TFRAMEfindCriteriaCodeDesc`: Componente para entrada de critérios de busca.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `paymentCode` (tipo: string, obrigatório).
  - `descrip` (tipo: string, opcional, máx. 255 caracteres).
  - `discount` (tipo: numérico, opcional, >= 0).
  - `dueDate` (tipo: data, opcional).
  - `discDate` (tipo: data, opcional).
  - `FInvDisp` (tipo: string, opcional).
  - `FInvDispDesc` (tipo: string, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Os campos exibidos na interface correspondem diretamente às colunas do banco de dados.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  var
    Form: TFORMLpaymentCodes;
  begin
    Form := TFORMLpaymentCodes.Create(nil);
    try
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
  ```
* **HTML Renderizado:**
  ```html
  <div style="font-family: Verdana; width: 100%;">
    <h3>Payment Codes List</h3>
    <table border="1" style="width: 100%; border-collapse: collapse;">
      <thead>
        <tr>
          <th>Status</th>
          <th>Payment Code</th>
          <th>Description</th>
          <th>Discount</th>
          <th>Due Date</th>
          <th>Discount Date</th>
          <th>Invoice Display</th>
          <th>Invoice Display Description</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>Active</td>
          <td>PC001</td>
          <td>Payment Code 1</td>
          <td>10%</td>
          <td>2023-12-31</td>
          <td>2023-12-15</td>
          <td>Display 1</td>
          <td>Description 1</td>
        </tr>
      </tbody>
    </table>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* `mc_GRID_FIELDS`: Define os campos exibidos na grade e sua ordem.
* `DefineHiddenFields('HIDE_ALL_FIELDS')`: Oculta todos os campos inicialmente.

---

## 12. Conclusão:

O código `LpaymentCodes` é uma implementação robusta para gerenciar códigos de pagamento em uma interface de lista. Ele oferece funcionalidades essenciais como busca, criação, modificação e visualização de registros. No entanto, a ausência de integração explícita com APIs limita sua escalabilidade em sistemas distribuídos.

---

## 13. Resumo Curto:

O código `LpaymentCodes` implementa uma interface para gerenciar códigos de pagamento, permitindo busca, criação, modificação e visualização de registros. Ele utiliza componentes visuais e manipulação de dados em Delphi, com foco em usabilidade e organização.#### **LpaymentCodes.pas**

```
unit LpaymentCodes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, Buttons, sSpeedButton, kneFRGridManager,
  ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter, kneCBList,
  sDBText, kneFRfindCriteria, kneFRfindCriteriaCodeDesc, sScrollBox,
  kneEnterAsTab, knePrivileges, sCheckBox;

type
  TFORMLpaymentCodes = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    DBTXTpaymentCode: TsDBText;
    DBTXTdescrip: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBTXTdiscount: TsDBText;
    DBTXTdueDate: TsDBText;
    DBTXTdiscDate: TsDBText;
    DBTXTFInvDisp: TsDBText;
    sLabel2: TsLabel;
    sBevel3: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    FRAMEfindCriteriaCodeDesc1: TFRAMEfindCriteriaCodeDesc;
    sLabel3: TsLabel;
    sBevel4: TsBevel;
    DBTXTFInvDispDesc: TsDBText;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    CHKactiveOnly: TsCheckBox;
  private
    { Private declarations }
  protected
    procedure GridSetup; override;
    procedure EventSetup; override;

    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;    
  end;

var
  FORMLpaymentCodes: TFORMLpaymentCodes;

implementation

uses
  PaymentServiceUtils, MPaymentCodes;

const
  mc_GRID_FIELDS = 'stat; paymentCode; descrip; discount; dueDate; ' +
    	'discDate; FInvDisp; FInvDispDesc; ordChklstdocs; giroPayment' ; // [01-04-2016, #22755] //11-06-2012, #13144

{$R *.dfm}

{ TFORMLpaymentCodes }

class function TFORMLpaymentCodes.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLpaymentCodes.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLpaymentCodes.EventSetup;
begin
  inherited;

end;

procedure TFORMLpaymentCodes.GridSetup;
begin
  inherited;

  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_GRID_FIELDS); // [01-04-2016, #22755]

```

#### **LpaymentCodes.dfm**

```
inherited FORMLpaymentCodes: TFORMLpaymentCodes
  Left = 346
  Top = 139
  Caption = 'Payment Codes List'
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLsearchArea: TsPanel
    inherited PNLsearchButtons: TsPanel
      TabOrder = 0
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        inline FRAMEfindCriteriaCodeDesc1: TFRAMEfindCriteriaCodeDesc
          Left = 1
          Top = 1
          Width = 689
          Height = 64
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          inherited LBLcode: TsLabel
            Width = 35
            ParentFont = True
          end
          inherited sLabel1: TsLabel
            Width = 69
            ParentFont = True
          end
          inherited EDTcode: TsEdit
            Left = 80
            ParentFont = True
          end
          inherited EDTdescription: TsEdit
            Left = 80
            ParentFont = True
          end
        end
        object CHKactiveOnly: TsCheckBox
          Left = 166
          Top = 10
          Width = 91
          Height = 19
          Caption = 'Active Only'
          Checked = True
          State = cbChecked
          TabOrder = 1
          SkinData.SkinSection = 'CHECKBOX'
          ImgChecked = 0
          ImgUnchecked = 0
        end
      end
    end
  end
  inherited PNLlist: TsPanel
    inherited PNLdetailArea: TsPanel
      inherited PNLviewer: TsScrollBox
        object LBL1: TsLabel
          Left = 8
          Top = 16
          Width = 57
          Height = 16
          Caption = 'Payment'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
        end
        object BVL2: TsBevel
          Left = 8
          Top = 35
          Width = 209
          Height = 9
          Shape = bsTopLine
        end
        object DBTXTpaymentCode: TsDBText
          Left = 24
          Top = 48
          Width = 25
          Height = 13
          Caption = 'Code'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'paymentCode'
          DataSource = DSRlist
        end
```
<!-- tabs:end -->

