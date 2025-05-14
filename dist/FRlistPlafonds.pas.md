<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código `FRlistPlafonds` implementa uma interface de exibição e edição de dados em formato de grid (grade). Ele é utilizado para gerenciar informações relacionadas a "Plafonds" (limites ou orçamentos) associados a agentes. A funcionalidade principal é permitir que os usuários visualizem, filtrem, editem e adicionem novos registros de forma eficiente.

* **Tecnologias Utilizadas:**
  - **Delphi Framework:** Utilizado para criar a interface gráfica e manipular eventos.
  - **Componentes cxGrid:** Para exibição de dados em formato de tabela.
  - **ClientDataSet (TClientDataSet):** Para manipulação de dados em memória.
  - **SOAP Services:** Para integração com serviços externos.
  - **Bibliotecas Personalizadas:** Como `kneUtils`, `kneTypes`, e `ProdmktServiceUtils`.

* **Forma do Componente:**
  - **Grid Display (Exibição em Grade):**
    - **Colunas da Grade e Tipos:**
      - `seqNum` (Número Sequencial, Inteiro).
      - `prodmktOrc` (Orçamento de Produto, String).
      - `format` (Formato, String).
      - `product` (Produto, String).
      - `shade` (Sombra, String).
      - `brandGroup` (Grupo de Marca, String).
      - `minVlEur` (Valor Mínimo em Euros, Decimal).
      - `maxVlEur` (Valor Máximo em Euros, Decimal).
      - `minTon` (Tonelagem Mínima, Decimal).
      - `maxTon` (Tonelagem Máxima, Decimal).
      - `commType` (Tipo de Comissão, String).
      - `commRate` (Taxa de Comissão, Decimal).
      - `fixValue` (Valor Fixo, Decimal).
    - **Ações da Grade e Efeitos:**
      - Adicionar (`ADD`): Insere um novo registro.
      - Excluir (`DELETE`): Remove o registro selecionado.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar novos registros.
  - Editar valores diretamente na grade.
  - Filtrar registros com base em critérios específicos.
  - Excluir registros existentes.

* **Componentes Principais:**
  - **Grid (cxGrid):** Exibe os dados em formato tabular.
  - **ClientDataSet (CDStable):** Gerencia os dados em memória.
  - **Botões de Ação:** Permitem interações como adicionar e excluir registros.
  - **Filtros Personalizados:** Como `cxEDTfindBrandGroup` e `cxEDTfindProdBudget`.

* **Tradução para Pseudo-Código:**
  - Evento `OnClick` do botão "Adicionar": `if botão clicado then insere novo registro`.
  - Evento `OnEditValueChanged` na grade: `if valor da célula alterado then atualiza registro`.
  - Evento `OnDataChange` no DataSource: `if dados alterados then atualiza exibição`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização:
     - O construtor `Create` configura as propriedades da grade e define os campos principais, ocultos e de ordem.
  2. Interações do Usuário:
     - O usuário pode clicar em botões para adicionar ou excluir registros.
     - Alterações nos valores das células disparam eventos para validação e atualização.
  3. Funções Executadas:
     - `ShowData` (Arquivo: `FRlistPlafonds`): Carrega os dados na grade.
     - `CDStableNewRecord` (Arquivo: `FRlistPlafonds`): Configura valores padrão para novos registros.
     - `ACTaddExecute` (Arquivo: `FRlistPlafonds`): Adiciona um novo registro.

* **Dados Necessários:**
  - Informações como `seqNum`, `prodmktOrc`, `format`, `product`, entre outros, devem ser preenchidas para criar ou editar registros.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas quando um registro está selecionado.

* **Filtros Disponíveis:**
  - Filtros por `BrandGroup`, `ProdBudget`, `Format`, `Shade`, e `Product`.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se um valor não atender aos critérios esperados.

* **Valores Padrão dos Campos:**
  - `commType`: Padrão "T".
  - `minVlEur` e `maxVlEur`: Padrão 0.

* **Validações e Condições dos Campos:**
  - `commType`: Deve ser "T" ou "%".
  - `minVlEur` e `maxVlEur`: Devem ser números positivos.

---

## 5. Funções Principais:

* **`ShowData`:** Carrega os dados na grade.
* **`CDStableNewRecord`:** Configura valores padrão para novos registros.
* **`ACTaddExecute`:** Adiciona um novo registro.
* **`CDStableBeforePost`:** Valida os dados antes de salvar.

---

## 6. Consumo de Serviços API:

* **Serviço:** `ProdmktServiceUtils`.
  - **Endpoint:** `/api/prodmkt`.
  - **Dados Enviados:** `{ "prodmktOrc": "string" }`.
  - **Dados Recebidos:** `{ "status": "success", "data": "Objeto Produto" }`.
  - **Propósito:** Buscar informações de orçamento de produto.
  - **Tratamento de Erros:** Exibe mensagem de erro em caso de falha.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.

* **Componentes Personalizados:**
  - `kneUtils`: Utilitários gerais.
  - `ProdmktServiceUtils`: Integração com serviços de produto.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `seqNum` (Inteiro, Obrigatório).
  - `prodmktOrc` (String, Obrigatório).
  - `commType` (String, Obrigatório, Valores Permitidos: "T", "%").
  - `minVlEur` (Decimal, Opcional).
  - `maxVlEur` (Decimal, Opcional).

* **Mapeamento:**
  - `seqNum` → Coluna `seqNum` no banco de dados.
  - `prodmktOrc` → Coluna `prodmktOrc` no banco de dados.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMElistPlafonds := TFRAMElistPlafonds.Create(Self);
  FRAMElistPlafonds.ShowData;
  ```
* **HTML Renderizado:**
  ```html
  <table style="border: 1px solid black; width: 100%;">
    <thead>
      <tr>
        <th>seqNum</th>
        <th>prodmktOrc</th>
        <th>format</th>
        <th>product</th>
        <th>shade</th>
        <th>brandGroup</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>1</td>
        <td>Orçamento A</td>
        <td>Formato X</td>
        <td>Produto Y</td>
        <td>Sombra Z</td>
        <td>Grupo 1</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração de Propriedades:**
  - `MasterKeyFields := 'Code=agent';` Define os campos principais.
  - `AvailableActions := 'ADD;DELETE';` Define as ações disponíveis.

---

## 12. Conclusão:

O código `FRlistPlafonds` é uma implementação robusta para gerenciar dados em formato de grade. Ele oferece funcionalidades de edição, adição e exclusão, além de integração com serviços externos. No entanto, a validação de campos poderia ser mais detalhada.

---

## 13. Resumo Curto:

O `FRlistPlafonds` é um componente de exibição e edição de dados em grade, com suporte a filtros e integração com serviços externos. Ele é ideal para gerenciar informações relacionadas a "Plafonds" de agentes.#### **FRlistPlafonds.pas**

```
unit FRlistPlafonds;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMElistPlafonds = class(TFRAMEBaseGridEditSOA)
    cxEDTfindBrandGroup: TcxEditRepositoryButtonItem;
    cxEDTfindProdBudget: TcxEditRepositoryButtonItem;
    cxICBcommType: TcxEditRepositoryImageComboBoxItem;
    cxEDTfindFormat: TcxEditRepositoryButtonItem;
    cxEDTfindshade: TcxEditRepositoryButtonItem;
    cxEDTfindproduct: TcxEditRepositoryButtonItem;
    procedure CDStableBeforeInsert(DataSet: TDataSet);
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure DStableDataChange(Sender: TObject; Field: TField);
    procedure CDStableBeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    mv_DataChangeEnabled : Boolean;
    mv_FieldNextVal: Integer;
    procedure m_FindByCodeBrandGroup(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_SetFindBrandGroup(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindProdBudget(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeProdBudget(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_setFindFormat(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeFormat(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeProduct(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeShade(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_setFindProduct(Sender: TObject; AButtonIndex: Integer);
    procedure m_setFindShade(Sender: TObject; AButtonIndex: Integer);
    procedure ShowData; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMElistPlafonds: TFRAMElistPlafonds;

implementation

uses
  kneUtils, kneTypes, Global, kneFindDialogSOA, kneFindDialog, kneDialogFactory,
  kneFGDBUtils, kneFGFindUtils, kneFREditSOA,
  //---ServiceUtils
  ProdmktServiceUtils, BrandGroupServiceUtils, BudgetFormatServiceUtils,
  BudgetShadeServiceUtils, BudgetProductServiceUtils, kneConfigObjects;

{$R *.dfm}

{ TFRAMElistPlafonds }

constructor TFRAMElistPlafonds.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'Code=agent';
  DataPacketName := 'AgentPlafond';
  PropertyName := 'plafonds';
  FrameType := frtDetail;

  AvailableActions := 'ADD;DELETE';

  ServiceParams.ShowInactives := True;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
//    kneUtils.TkneGeneric.SplitString('marketName;', ReadOnlyFields, ';', True);
    // Campos Hidden ...........................................................
    HiddenFields.Clear;
    kneUtils.TkneGeneric.SplitString('agent;', HiddenFields, ';', True);
    // Ordem Campos ............................................................
//    OrderFields.Clear;
    DefineOrderFields('seqNum;prodmktOrc;format;product;shade;brandGroup;minVlEur;maxVlEur;minTon;maxTon;commType;commRate;fixValue');
//    kneUtils.TkneGeneric.SplitString('seqNum', OrderFields, ';', True);
    // Key Fields ..............................................................
    KeyFields:= 'agent;seqNum';
    // Custom Editors ..........................................................
    AddCustomField('brandGroup','cxEDTfindBrandGroup');
    AddCustomField('prodmktOrc','cxEDTfindProdBudget');
```

#### **FRlistPlafonds.dfm**

```
inherited FRAMElistPlafonds: TFRAMElistPlafonds
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited CDStable: TClientDataSet
    OnNewRecord = CDStableNewRecord
  end
  inherited DStable: TDataSource
    OnDataChange = DStableDataChange
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindBrandGroup: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
    object cxEDTfindProdBudget: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
    object cxICBcommType: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'Ton'
          ImageIndex = 0
          Value = 'T'
        end
        item
          Description = '%'
          Value = '%'
        end>
    end
    object cxEDTfindFormat: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
```
<!-- tabs:end -->

